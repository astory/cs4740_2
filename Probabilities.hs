import Data.Ratio
import Ngram
import System.IO
import System.IO.Unsafe
import Tagging

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as B
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S

type CountMap = M.Map String (M.Map String Int)

{-convert lists of sentences to dictionaries which can give counts of words
given the tag-}
add_pair_to_map :: CountMap -> (String, String) -> CountMap
add_pair_to_map map (tag, word) =
        case M.lookup tag map of
            Nothing ->
                M.insert tag (M.singleton word 1) map
            Just tagmap ->
                M.insert tag (M.insertWith (+) word 1 tagmap) map

val'key :: [[(String, String)]] -> CountMap
val'key = L.foldl' (\ map sent -> L.foldl' add_pair_to_map map sent) M.empty

default_tag = "NNP"

swap (a,b) = (b,a)
second = \ (_,a) (_,b) -> b `compare` a -- for sorting descending

pick_most_frequent :: CountMap -> String -> String
pick_most_frequent map tag =
    case M.lookup tag map of
        Nothing ->
            default_tag
        Just tagmap ->
        -- head is unsafe; the way we build the maps guarantees they are not empty
            fst . head . L.sortBy second . M.toList $ tagmap

initial_prob :: String -> Rational
initial_prob "<s>" = 1
initial_prob _ = 0

sum_countmap :: Ord a => Num b => M.Map a b -> b 
sum_countmap m = foldr (\ (_, count) acc -> acc + count) 0 (M.toList m)

ngram_prob :: Ord a => Bool -> [Int] -> [M.Map [a] Int] -> a -> [a] -> Rational
ngram_prob smooth gram_counts ngrams x prefix =
    -- we didn't see the prefix at all, 0 is sensible, to avoid breaking
    if denominator == 0 then
        0
    else
        (toInteger numerator) % (toInteger denominator)
    where
        len = length prefix
        whole_map = ngrams !! (len + 1)
        prefix_map = ngrams !! len
        numerator = 
            let count = B.fromMaybe 0 (M.lookup (prefix ++ [x]) whole_map) in
            if smooth then
                count + 1
            else count
        denominator =
            let count = B.fromMaybe 0 (M.lookup (prefix) prefix_map) in
            if smooth then
                count + gram_counts !! (len + 1)
            else count

tag_map_sum :: Ord a => Ord b => Num c => M.Map a (M.Map b c) -> a -> c
tag_map_sum map tag =
    case M.lookup tag map of
        Nothing -> 0
        Just tagmap -> sum_countmap tagmap

tag_probability :: Bool -> S.Set String -> (String -> Int) -> CountMap -> String -> String -> Rational
tag_probability unk observed_words tag_sum word'tag word tag =
    case M.lookup tag word'tag of
        Nothing -> 0 -- unknown tag, shouldn't happen
        Just tagmap ->
            case M.lookup word tagmap of
                Nothing ->
                    if unk then
                        case M.lookup "<UNK>" tagmap of
                            Nothing -> 0
                            Just count ->
                                toInteger(count) % toInteger(tag_sum tag)
                    else
                        -- We didn't find any instances of <tag, word>
                        if S.notMember word observed_words && tag == "NNP" then
                            1
                        else
                            0
                Just count ->
                    toInteger(count) % toInteger(tag_sum tag)

readable = map (\(a,b) -> (fromRational a, b))

upsl = unsafePerformIO . putStrLn

viterbi :: Bool -> Bool ->
        S.Set String -> CountMap -> [M.Map [String] Int] -> [Int] ->
        [[String]] ->
        [[String]]
viterbi unk smooth
        observed_words word'tag taggrams gram_counts
        all_words =
    map v_sentence all_words where
        unigrams = taggrams !! 1
        taglist = L.concat . S.elems $ M.keysSet unigrams
        tag_sum = (Memo.list Memo.char) (tag_map_sum word'tag)
        tag_prob = tag_probability unk observed_words tag_sum word'tag
        taggram_prob = ngram_prob smooth gram_counts taggrams
        n = toInteger $ length taggrams - 1
        v_sentence words = ks where
            w n = words !! (fromInteger n - 1)
            v :: Integer -> String -> (Rational, [String])
            v = Memo.memo2 Memo.integral (Memo.list Memo.char) v'
                where
                v' 1 k = (tag_prob (w 1) k * initial_prob k, [k])
                v' t k =
                    --upsl ("v' " ++ show(t) ++ " " ++ k )`seq`
                    let 
                        measure_tag :: String -> (Rational, [String])
                        measure_tag (y) =
                            let (value, ks) = v (t-1) y -- ks ends in y
                                trans_prob = taggram_prob y (take (fromInteger(n-1)) (init ks))
                            in 
                                --upsl ("value,ks,trprb,n" ++ show(value, ks, fromRational trans_prob,n)) `seq`
                                (trans_prob * value, ks)
                        options = map measure_tag taglist :: [(Rational, [String])]
                        (best, ks) = 
                            L.maximumBy (\(a,_) (b,_) -> a `compare` b) options
                        lex_prob = tag_prob (w t) k
                    in 
                        --upsl ("options: "++ (show (readable options))) `seq`
                        (lex_prob * best, ks ++ [k])
            options = map (\y -> v (toInteger . length $ words) y) taglist
            (best, ks) = L.maximumBy (\(a,_) (b,_) -> a `compare` b) options

count_dict :: Ord k => M.Map k Int -> Int
count_dict m = M.fold (+) 0 m


main = do
    training <- getContents
    withFile "pos_corpora/test-obs.pos" ReadMode (\handle -> do 
        test <- hGetContents handle
        let unk = True 
            smooth = False-- add-one smoothing
            tagged_words = (if unk then unktags else (\x->x)) $ posTag training
            sents = sentences tagged_words
            sents' = map (map swap) sents
            word'tag = val'key sents
            tag'word = val'key sents'

            (tags, words) = split_tags sents
            taggrams = safe_ngram_tally 3 tags
            gram_counts = map count_dict taggrams
            observed_words = S.fromList (concat words)

            test_words = (lines test)
            test_sents = (single_sentences test_words)
            t = test_sents
            output_tags = 
                observed_words `seq` word'tag `seq` taggrams `seq` gram_counts
                `seq` putStrLn "read dataset" `seq`
                concat $ viterbi unk smooth observed_words word'tag taggrams gram_counts t
        putStrLn $ unlines . map show $ zip output_tags (concat t)
        hClose handle
        )
