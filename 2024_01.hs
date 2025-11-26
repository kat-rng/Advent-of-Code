import Data.List
import System.IO

-- Take the sum of the absolute differences
totalDiff :: [[Integer]] -> Integer
totalDiff x = sum (map abs (zipWith (-) (sort (head x)) (sort (last x))))

-- Create a list of two integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map readAsInt (words line)

-- Read the string as integers
readAsInt :: String -> Integer
readAsInt x = read x

-- Compare two lists
main = do
    --let l1 = [3,4,2,1,3,3]
    --let l2 = [4,3,5,3,9,3]
    --putStrLn (show (totalDiff l1 l2))

    -- Test reading out the file
    handle <- openFile "2024_01_input1" ReadMode
    contents <- hGetContents handle
    let x = lines contents
    let matrix = map lineToInts x
    let result = totalDiff (transpose matrix)
    putStr (show result)
