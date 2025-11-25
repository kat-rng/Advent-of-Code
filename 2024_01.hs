import Data.List
main :: IO ()

-- Take the sum of the absolute differences
totalDiff x y = sum (map abs (zipWith (-) (sort x) (sort y)))

-- Compare two lists
main = do
    let l1 = [3,4,2,1,3,3]
    let l2 = [4,3,5,3,9,3]
    putStrLn (show (totalDiff l1 l2))