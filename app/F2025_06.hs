module F2025_06 (pt1, pt2) where

import System.IO
import Data.List

-- read the input into a set of columns of numbers (ns) and operations (os)
readInput :: String -> ([[Integer]], String)
readInput s = do
    let xs = map words $ lines s
    let ns = map (map read) $ init xs
    let os = map head $ last xs
    (transpose ns, os)

-- zip the lists into tuples of "math problems"
readTuples :: ([[Integer]], String) -> [([Integer], Char)]
readTuples (ns, os) = zip ns os

-- Apply the operation defined by the char on the integers
applyOperation :: ([Integer], Char) -> Integer
applyOperation (ints, op) = case op of
    '*' -> product  ints
    '+' -> sum      ints
    _   -> error "invalid operation"

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_06_input" ReadMode
    contents <- hGetContents handle

    -- Get the total fresh
    let input = readTuples $ readInput contents
    let output = sum $ map applyOperation input
    -- print it
    putStr $ show output

pt2 :: IO ()
pt2 = do

    putStr "Nothing"

