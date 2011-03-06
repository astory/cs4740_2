import System.IO
import Data.Char  
import Data.List.Split

--Turn a string of tags and words into
--a list of a list of tag and word
sepWords :: String -> [String]
sepWords []=[]
sepWords x=splitOn "\n" x

sepTags :: [String] -> [[String]]
sepTags []=[]
sepTags (x:xs) = (splitOn " " x):sepTags xs


rawwords="<s> <s>\nNNP Pierre\nNNP Vinken\n, ,\nCD 61\nNNS years\nJJ old\n, ,\nMD will\nVB join\nDT the\nNN board\nIN as\nDT a\nJJ nonexecutive\nNN director\nNNP Nov.\nCD 29\n. ."
--test="pos_corpora/test-obs.pos"
--train="pos_corpora/train.pos"

--ngram :: Integer -> String -> 
{-
ngram n words=
	wordlist=splitOn "\n" contents
	head wordlist
	def ngram(n, words):
	"""Return a list of n (1 .. n)-gram dictionaries, n >= 1, l[0] is {}, where
	the keys are tuples of words"""
	ngrams = [{}]
	for i in range(1, n+1):
		d = {}
		# compute the i-gram model data
		word_buffer = words[0:i-1]
		for word in words[i-1:]:
			word_buffer.append(word)
			t = tuple(word_buffer)
			if d.has_key(t):
				d[t] += 1
			else:
				d[t] = 1
			word_buffer.pop(0)
		ngrams.append(d)
	return ngrams
-}

--main = do
--	contents <- readFile train

--	writeFile "foo.pos" contents
