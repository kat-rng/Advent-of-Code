import Data.List
import System.IO

-- Take the sum of the absolute differences
totalDiff x y = sum (map abs (zipWith (-) (sort x) (sort y)))

readFixed 
    

-- Compare two lists
main = do
    --let l1 = [3,4,2,1,3,3]
    --let l2 = [4,3,5,3,9,3]
    --putStrLn (show (totalDiff l1 l2))

    -- Test reading out the file
    handle <- openFile "2024_01_input1" ReadMode
    contents <- hGetContents handle
    let x = lines contents
    let y = map words x
    putStr (head (head y))
