import Math.NumberTheory.Logarithms
import Data.List
import System.IO
import Data.List.Split

-- Finds repetition in integers
isRepeatedDiv :: Integer -> Integer -> Bool
isRepeatedDiv n slices
    | size `mod` slices /= 0 = False
    | slices /= 2 && mid == upper = isRepeatedDiv lower (slices-1)
    | slices /= 2 = False
    | upper == lower = True
    | otherwise = False
    where 
        size   = fromIntegral (1 + integerLog10 n)
        slicer = (10 ^ (size - size `div` slices))
        slicerMid = (10 ^ (size - 2 * size `div` slices))
        upper = n `div` slicer
        lower = n `mod` slicer
        mid   = lower `div` slicerMid

readDeep :: [String] -> [Integer]
readDeep = map read

allRepeatedDiv :: Integer -> Bool
allRepeatedDiv =
    where 
        maxSize = fromIntegral (1 + integerLog10 n)

returnDiv :: Integer -> Integer
returnDiv n
    | isRepeatedDiv n 2 = n
    | otherwise       = 0

seekAllInRange :: [Integer] -> Integer
seekAllInRange xs = sum $ map returnDiv [(head xs) .. (last xs)]

pt1 = do
    -- Reading from the file
    handle <- openFile "2025_02_input" ReadMode
    contents <- hGetContents handle
    let x1 = splitOn "," contents
    let x2 = map (splitOn "-") x1
    let x3 = map readDeep x2
    
    let total = sum $ map seekAllInRange x3

    putStr $ show total

