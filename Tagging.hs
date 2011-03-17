module Tagging
( posTag
, unkFirst
) where

import System.IO
import Data.Char
import Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M


posTag :: String -> [(String,String)]
posTag = map ((\(h:t:xs) -> (h,t)) . words) . lines

{-
def unk(words):
	#Add unknown tokens to the first occurance of each word
	#in the list of word tokens words.
	unks=[]
	for i in range(0,len(words)-1):
		if words[i] not in unks:
			unks.append(words[i])
			words[i]=UNK
	return words

-}
--Convert the first occurrance of a tag-word pair to a tag-<UNK> pair
--unkFirst :: [(String,String)] -> [(String,String)]
unkFirst corpus =  reverse $ fst . snd $ unkStep (["<s>"],( [head corpus] ,tail corpus ) )
--unkFirst corpus = corpus

unkStep input 
      | snd (snd input) == []  = input  -- reverse $ snd $ fst $ input
      | otherwise = unkStep $ checkWord input 


checkWord :: ([String], ( [(String,String)],[(String,String)] ) ) -> ( [String], ( [(String,String)],[(String,String)] ) )
checkWord input
      | word `L.notElem` removed = ( word:removed , ( (tag, unk):unked , tail notUnked ) )
    --  | sort ( intersect removed notUnked_words ) == sort ( removed ) = ( removed , (  (reverse notUnked) ++ unked  , [("NN","<end>")] ) )
      | word `L.elem` removed    = ( removed , ( (tag, word):unked , tail notUnked ) )
      where unk  = "<UNK>"
            removed = fst input
            corpus = snd input
            unked = fst corpus
            notUnked = snd corpus
            notUnked_words = L.map fst notUnked
            lexeme = head notUnked
            word = snd lexeme
            tag = fst lexeme



main = do
	text <- getContents
	let tagged_words = posTag text --unkFirst [] (posTag text) [("a","b")]
	    (tags, _) = unzip tagged_words
	print $ unkFirst $ posTag text
