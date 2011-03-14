module Tagging
( posTag
) where

import System.IO
import Data.Char
import Data.List as L

posTag :: String -> [(String,String)]
posTag = map ((\(h:t:xs) -> (h,t)) . words) . lines

{-
--Convert the first occurrance of a tag-word pair to a tag-<UNK> pair
unkFirst :: [String] -> [(String,String)] -> [(String,String)] -> [(String,String)]
unkFirst removed corpus unked
       | (head corpus) `L.notElem` removed = --First occurrance
         unkFirst removed:(head corpus) (tail corpus) unked:((fst . head) corpus, "<UNK>")
       | (head corpus) `L.elem` removed = --Later occurrance
         unkFirst removed (tail corpus) unked:(head corpus)
-}

main = do
	text <- getContents
	let tagged_words = posTag text --unkFirst [] (posTag text) [("a","b")]
	    (tags, _) = unzip tagged_words
	print $ posTag text
