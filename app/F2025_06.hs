module F2025_06 (pt1, pt2) where

import System.IO
import Data.List
import Control.Exception (handle)

-- read the input into a set of columns of numbers (ns) and operations (os)
readInput :: String -> ([[Integer]], [Char])
readInput s = do
    let xs = map words $ lines s
    let ns = map (map read) $ init xs
    let os = map head $ last xs
    (transpose ns, os)

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_05_input" ReadMode
    contents <- hGetContents handle

    -- Get the total fresh
    let input = readInput contents

    -- print it
    putStr $ show input

pt2 :: IO ()
pt2 = do
    -- Reading from the file
    handle <- openFile "2025_05_input" ReadMode
    contents <- hGetContents handle

    putStr "Nothing"

