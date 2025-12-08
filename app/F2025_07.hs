module F2025_07 (pt1, pt2) where

import System.IO
import Data.List
import Debug.Trace

-- Finds the start position and starts the recursive search
processListStart :: [String] -> [(Int, Int)]
processListStart xs = do
    let startIndex = elemIndex 'S' $ head xs
    case startIndex of
        Nothing     -> error "No start point"
        Just num    -> recursiveSplit xs 1 num []

-- recursively checks through the array to find splits
-- Returns a list of every coordinate of split found
recursiveSplit ::  [String] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
recursiveSplit lists layerN i cull = case mLists of
    Nothing -> []
    -- If there is a layer here then solve it
    Just jLists -> solveCurrentLayer jLists layerN i cull
    where 
        mLists = uncons lists

-- Identify if there is a split at this index
-- if so send it to the handlers
solveCurrentLayer :: (String, [String]) -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
solveCurrentLayer (currentList, nextLists) layerN i cull = do 
    if isSplit then
        -- check if this branch has already been searched
        if (i, layerN) `elem` cull then
            []
        else
            -- if not then continue
            (i, layerN) : handleSplit nextLists i layerN cull
    else
        recursiveSplit nextLists (layerN+1) i cull
    where isSplit = '^' == currentList !! i 

-- splits the lines of search
handleSplit :: [String] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
handleSplit nextLists i layerN cull = do
    let l1 = currSplit (i-1) cull
    let l2 = currSplit (i+1) (l1++cull)
    l1 ++ l2
    where 
        currSplit = recursiveSplit nextLists (layerN+1)


pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_07_input" ReadMode
    contents <- hGetContents handle
    let input = lines contents

    let output = processListStart input

    let uniqueSplits = nub output

    putStr $ show $ length uniqueSplits

pt2 :: IO ()
pt2 = do
    putStr $ show "Nothing"
