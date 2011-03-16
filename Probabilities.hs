import System.IO
import System.IO.Unsafe
import Ngram
import Tagging
import Data.Ratio

import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

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

viterbi :: CountMap -> [M.Map [String] Int] -> [String] -> Rational
viterbi word'tag taggrams words =
    let unigrams = head taggrams
        taglist = L.concat . S.elems $ M.keysSet unigrams
        w n = words !! (fromInteger n - 1)
        tag_prob :: String -> String -> Rational
        tag_prob word tag =
            case M.lookup tag word'tag of
                Nothing -> 0 -- unknown tag
                Just tagmap ->
                    case M.lookup word tagmap of
                        Nothing -> 0 -- smoothing here
                        Just count ->
                            toInteger(count) % toInteger(sum_countmap tagmap)
        v :: Integer -> String -> Rational
        v 1 k = tag_prob (w 1) k * initial_prob k
        v t k =
            unsafePerformIO(print (t, k)) `seq`
            let 
                measure_tag y = {-transition prob-} v (t-1) y
                options = map measure_tag taglist
                best = maximum options
            in tag_prob (w t) k * best
        v' = Memo.memo2 Memo.integral (Memo.list Memo.char) v
        options = map (\y -> v' (toInteger . length $ words) y) taglist
        best = maximum options :: Rational
    in best

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
            taggrams = build_ngram_tally 1 tags

            test_words = lines test
            test_sents = (single_sentences test_words)
        word'tag `seq` taggrams `seq` print "read dataset"
        print $ viterbi word'tag taggrams (take 2 (head test_sents))
        hClose handle
        )
