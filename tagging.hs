import System.IO
import Data.Char
import Data.List.Split

import NLPWP

--import Data.HMM

--Turn a string of tags and words into
--a list of a list of tag and word
sepWords :: String -> [String]
sepWords []=[]
sepWords x=splitOn "\n" x

sepTags :: [String] -> [[String]]
sepTags []=[]
sepTags (x:xs) = (splitOn " " x):sepTags xs


--rawwords="<s> <s>\nNNP Pierre\nNNP Vinken\n, ,\nCD 61\nNNS years\nJJ old\n, ,\nMD will\nVB join\nDT the\nNN board\nIN as\nDT a\nJJ nonexecutive\nNN director\nNNP Nov.\nCD 29\n. ."
rawwords="<s>\nRockwell\nInternational\nCorp.\n's\nTulsa\nunit\nsaid\nit\nsigned\na\ntentative\nagreement\nextending\nits\ncontract\nwith\nBoeing\nCo.\nto\nprovide\nstructural\nparts\nfor\nBoeing\n's\n747\njetliners\n.\n<s>"
--test="pos_corpora/test-obs.pos"
--train="pos_corpora/train.pos"


{-
main = do
--	rawwords <- readFile test
	contents = foldl countElem Data.Map.empty (sepWords rawwords)
	print contents
	--writeFile "foo.pos" contents

-}
