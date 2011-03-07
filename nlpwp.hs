--module NLPWP
--Code in this file is from http://nlpwp.org/book/chap-words.xhtml
import qualified Data.Map
countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = case (Data.Map.lookup e m) of
                  Just v  -> Data.Map.insert e (v + 1) m
                  Nothing -> Data.Map.insert e 1 m

freqList :: (Ord k) => [k] -> Data.Map.Map k Int
freqList = foldl countElem Data.Map.empty

bigram :: [a] -> [[a]]
bigram xs = if length(xs) >= 2
	then take 2 xs : bigram (tail xs)
	else []
