import System.IO
import Data.Char  

test='pos_corpora/test-obs.pos'
train='pos_corpora/train.pos'

main = do     
	contents <- readFile test
	writeFile 'foo.pos'
