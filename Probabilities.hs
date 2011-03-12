import System.IO
import Ngram
import Tagging

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

type CountMap = M.Map String (M.Map String Int)
type Prob = Float
type LogProb = Float


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

baseline = do
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
{-
pick_viterbi :: CountMap -> String -> String
pick_viterbi map tag =
  -}

--Log probabilities
toLog p = (log . fromIntegral) p
probdiv num denom = toLog  num - toLog denom

--Lexical generation probabilities (emission probabilities) for a particular word|tag
--Denominator
lexDenom counts sum = sum + counts

--Everything else
lexical :: CountMap -> String -> String -> LogProb
lexical word'tag word tag
       --If the tag occurs in the training corpus
      | M.notMember tag word'tag = 0
       --If the word occurs with that tag in the training corpus
      | M.notMember word (word'tag M.! tag) = 0
      -- Otherwise, the probability of the word given the tag
      | otherwise = (word'tag M.! tag M.! word) `probdiv` (M.fold lexDenom 0 (word'tag M.! tag)) 

--M.map (lexical "elephant" word'tag) ((S.elems . M.keysSet) word'tag)



--(S.elems . M.keysSet) word'tag

--let word'tag = (val'key . sentences) [("<s>","<s>"),("NN","director"),("NN","elephant")]
main' = do
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

main = do
    training <- getContents
    withFile "pos_corpora/test-obs_short.pos" ReadMode (\handle -> do 
        test <- hGetContents handle
        let tagged_words = posTag training
            sents = sentences tagged_words
            sents' = map (map swap) sents
            word'tag = val'key sents
            tag'word = val'key sents'

            test_words = lines test
        --Print the predictions
        --putStrLn $ unlines . (map show) . zip test_words . map (pick_most_frequent tag'word) $ test_words

        --Print just the tags that were guessed
        --print $ map snd $ zip test_words . map (pick_most_frequent tag'word) $ test_words
        
        --Print the transition probabilities
        print $ map ( lexical word'tag "director") $ map snd $ zip test_words . map (pick_most_frequent tag'word) $ test_words
        
        --print ( lexical word'tag "director" "NN" )
        hClose handle
        )



