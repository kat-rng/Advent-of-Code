module F2025_04 (pt1, pt2) where

import Prelude as P
import Data.Massiv.Array as A
import System.IO

charToInt :: Char -> Int
charToInt c = case c of
    '.' -> 0
    '@' -> 1
    _   -> error "Invalid character"

-- Converts a string representation of the input into a Massiv array
inputToLists :: String -> Array U Ix2 Int
inputToLists xs = A.fromLists' A.Seq intLists
    where intLists = P.map (P.map charToInt) (lines xs)

-- Create a stencil that finds the total neighbors that are paper rolls for each point
sum3x3Stencil :: Integral a => Stencil Ix2 a a
sum3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) 
    (\get ->
         get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1)
      +  get ( 0 :. -1) +                 get ( 0 :. 1)
      +  get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)
      
    )
{-# INLINE sum3x3Stencil #-}

-- Creating a padding to ensure the output from the stencil operation 
-- will be the same dimensions as the input
boxPad0 :: Padding Ix2 Int
boxPad0 = Padding (Sz2 1 1) (Sz2 1 1) (Fill 0)

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle

    -- Get the list contents
    let arr = inputToLists contents

    -- Pad the array
    let padArr = applyStencil boxPad0 sum3x3Stencil arr

    -- Determine accessibility
    let isAccessible = A.map (\x -> fromEnum (x<4)) (A.computeAs U padArr)
    let isAccessibleU = A.computeAs U isAccessible

    --Find the total accessible paper rolls
    let totalAcc =  A.sum $ A.zipWith (*) isAccessibleU arr 
    putStr $ show totalAcc

pt2 = do
    Nothing
