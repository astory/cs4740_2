import System.IO
import Data.Char
import Data.List as L

posTag :: String -> [(String,String)]
posTag = map (\(h:t:xs) -> (h,t)) . map words . lines

main = do
	text <- getContents
	print $ posTag text

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
