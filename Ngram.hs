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
import qualified Data.Map as M
import Data.List as L
import Data.List.Split as S
import Tagging

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

{- takes a list of sentences of words and n and produces up to the n-grams with
those words, respecting sentence boundaries -}
full_ngrams :: Int -> [[a]] -> [[[a]]]
full_ngrams n xs
    | n <= 0 = []
    | otherwise =
        let l = [1 .. n] :: [Int]
        -- [[a]] -map-> [[[a]]] -concat-> [[a]]
        --   ^  ngram k  ^^^^^              ^
        -- sentences |all ngrams in     |  just a list of ngrams
        --           |lists per sentence|
        in map (\k->concat . map (ngrams k) $ xs) l

tally :: Ord a => [a] -> M.Map a Int
tally =
    foldr (\ ng map -> M.insertWith (+) ng 1 map) M.empty

build_ngram_tally :: Ord a => Int -> [[a]] -> [M.Map [a] Int]
build_ngram_tally n =
    map tally . full_ngrams n

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
    print $ build_ngram_tally 1 $ tags `seq` words
