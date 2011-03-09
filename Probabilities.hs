import System.IO
import Ngram
import Tagging

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
second = \ (_,a) (_,b) -> a `compare` b

pick_most_frequent :: CountMap -> String -> String
pick_most_frequent map tag =
    case M.lookup tag map of
        Nothing ->
            default_tag
        Just tagmap ->
        -- head is unsafe; the way we build the maps guarantees they are not empty
            fst . head . L.sortBy second . M.toList $ tagmap

main = do
    training <- getContents
    withFile "pos_corpora/test-obs.pos" ReadMode (\handle -> do 
        test <- hGetContents handle
        let tagged_words = posTag training
            sents = sentences tagged_words
            sents' = map (map swap) sents
            word'tag = val'key sents
            tag'word = val'key sents'

            test_words = lines test
        putStrLn $ unlines . map show . zip test_words . map (pick_most_frequent tag'word) $ test_words
        hClose handle
        )