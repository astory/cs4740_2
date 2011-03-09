module Ngram
( Kgram
, ngrams
, unigram
, bigram
, trigram
, split_tags
, sentences
) where

--Code in this file is from http://nlpwp.org/book/chap-words.xhtml
import qualified Data.Map
import Data.List as L
import Data.List.Split as S
import Tagging
countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = case (Data.Map.lookup e m) of
                  Just v  -> Data.Map.insert e (v + 1) m
                  Nothing -> Data.Map.insert e 1 m

freqList :: (Ord k) => [k] -> Data.Map.Map k Int
freqList = foldl countElem Data.Map.empty

type Kgram a = [a] -> [[a]]

ngrams :: Int -> Kgram a
ngrams 0 _ = []
ngrams _ [] = []
ngrams n xs
    | length xs >= n = take n xs : ngrams n (tail xs)
    | otherwise      = []

unigram :: Kgram a
unigram = ngrams 1

bigram :: Kgram a
bigram = ngrams 2

trigram :: Kgram a
trigram = ngrams 3

isStart :: (String,String) -> Bool
isStart (t,w) = t == "<s>"

{- Breaks list of tag,word pairs up by sentences -}
sentences :: [(String, String)] -> [[(String, String)]]
sentences =
    let splitter = S.dropInitBlank . S.keepDelimsL $ S.whenElt isStart
    in S.split splitter

{- converts a list of sentences with tag,word pairs to a pair of lists of
    sentences of tags, words -}
split_tags :: [[(a,b)]] -> ([[a]], [[b]])
split_tags = unzip . map unzip

main = do
    text <- getContents
    let tagged_words = posTag text
        sentences' = sentences tagged_words
        (tags, words) = split_tags sentences'
    putStrLn $ unlines . map show $ sentences'
