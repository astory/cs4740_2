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

sum_countmap :: M.Map a Int -> Int
sum_countmap m =
    foldr (\ (_, count) acc -> acc + count) 0 (M.toList m)

ngram_prob :: Ord a => Show a =>[M.Map [a] Int] -> a -> [a] -> Rational
ngram_prob ngrams x prefix =
    --unsafePerformIO(putStrLn(show(prefix ++ [x]) ++ ":" ++ show(numerator% denominator))) `seq`
    if denominator == 0 then
        -- we didn't see the prefix at all, 0 is sensible, to avoid breaking
        0
    else
        (toInteger numerator) % (toInteger denominator)
    where
        len = length prefix
        whole_map = ngrams !! (len + 1)
        prefix_map = ngrams !! len
        numerator = 
            B.fromMaybe 0 (M.lookup (prefix ++ [x]) whole_map)
        denominator =
            B.fromMaybe 0 (M.lookup (prefix) prefix_map)

readable = map (\(a,b) -> (fromRational a, b))

upsl = unsafePerformIO . putStrLn

viterbi :: S.Set String -> CountMap -> [M.Map [String] Int] -> [String] -> (Rational, [String])
viterbi observed_words word'tag taggrams words =
    let unigrams = taggrams !! 1
        taglist = L.concat . S.elems $ M.keysSet unigrams
        w n = words !! (fromInteger n - 1)
        tag_prob :: String -> String -> Rational
        tag_prob word tag =
            case M.lookup tag word'tag of
                Nothing -> 0 -- unknown tag, shouldn't happen
                Just tagmap ->
                    case M.lookup word tagmap of
                        Nothing -> 1 % 100000000 -- smoothing here
                        Just count ->
                            toInteger(count) % toInteger(sum_countmap tagmap)
        taggram_prob = ngram_prob taggrams
        n = toInteger $ length taggrams - 1
        v :: Integer -> String -> (Rational, [String])
        v = Memo.memo2 Memo.integral (Memo.list Memo.char) v'
            where
            v' 1 k = (tag_prob (w 1) k * initial_prob k, [k])
            v' t k =
                upsl ("v' " ++ show(t) ++ " " ++ k )`seq`
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
                    lex_prob = if S.member (w t) observed_words then
                            --upsl ("we have seen " ++ (w t)) `seq`
                            tag_prob (w t) k
                        else
                            --upsl ("unseen" ++ (w t)) `seq`
                            if k == "NNP" then 1 else 0
                in 
                    --upsl ("options: "++ (show (readable options))) `seq`
                    (lex_prob * best, ks ++ [k])
        options = map (\y -> v (toInteger . length $ words) y) taglist
        {- if the test word didn't appear in the training set, you get the last
        one in the list because we don't have smoothing, also, this should
        probably be a special case -}
        (best, ks) = L.maximumBy (\(a,_) (b,_) -> a `compare` b) options
    in 
        (best, ks)

main = do
    training <- getContents
    withFile "pos_corpora/test-obs_short.pos" ReadMode (\handle -> do 
        test <- hGetContents handle
        let tagged_words = posTag training
            sents = sentences tagged_words
            sents' = map (map swap) sents
            word'tag = val'key sents
            tag'word = val'key sents'

            (tags, words) = split_tags sents
            taggrams = safe_ngram_tally 2 tags
            observed_words = S.fromList (concat words)

            test_words = (lines test)
            test_sents = take 1 (single_sentences test_words)
            --t = ["<s>", "Kent", "cigarette", "filters"]
            t = head test_sents
        word'tag `seq` taggrams `seq` print "read dataset"
        print $ viterbi observed_words word'tag taggrams t
        print $ t
        hClose handle
        )
