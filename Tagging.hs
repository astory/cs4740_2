module Tagging
( posTag
, unktags
) where

import System.IO
import Data.Char
import Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M


posTag :: String -> [(String,String)]
posTag = map ((\(h:t:xs) -> (h,t)) . words) . lines

unk :: [String] -> [String]
unk l = list 
    where
        (_, list) = L.foldr f (S.singleton("<s>"),[]) l
        f word (set, list) = 
            if S.member word set then
                (set, word:list)
            else
                (S.insert word set, "<UNK>":list)

unktags pairs = zip tags uwords
    where
        (tags, words) = unzip pairs
        uwords = unk words

main = do
    text <- getContents
    let tagged_words = posTag text --unkFirst [] (posTag text) [("a","b")]
        (tags, _) = unzip tagged_words
    print $ unktags $ posTag text
