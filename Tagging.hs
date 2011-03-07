module Tagging
( posTag
) where

import System.IO
import Data.Char
import Data.List as L

posTag :: String -> [(String,String)]
posTag = map (\(h:t:xs) -> (h,t)) . map words . lines

main = do
	text <- getContents
	let tagged_words = posTag text
	    (tags, _) = unzip tagged_words
	print $ posTag text
