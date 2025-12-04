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

average3x3Stencil :: Integral a => Stencil Ix2 a a
3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ get ->
        (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
           get ( 0 :. -1) +                 get ( 0 :. 1) +
           get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   )
{-# INLINE average3x3Stencil #-}

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle
    let arr = inputToLists contents
    let 
    putStr $ show arr

pt2 = do
    Nothing
