import Data.List
import System.IO

-- Create a list of integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map read $ words line

diffList :: [Integer] -> [Integer]
diffList l = zipWith (-) (init l) (tail l)

data ReactorDirection = Unknown
        | Increasing
        | Decreasing
    deriving (Eq, Show)

-- Figure out if the reactor is increasing or decreasing
getState :: Integer -> ReactorDirection
getState change
    | change >  0 = Increasing
    | change <  0 = Decreasing
    | otherwise   = undefined

-- Check if the reactor is unsafe and engage in recursion
checkState :: Integer -> ReactorDirection -> [Integer] -> Bool
checkState stability direction differences
    | stability == 0                                    = False
    | length differences == 0                           = True
    | isUnsafeDiff currentDiff                          = checkStateFailed
    | direction == Unknown || direction == currentDir   = checkState stability currentDir (tail differences)
    | direction /= currentDir                           = checkStateFailed
    | otherwise                                         = undefined
    where 
        currentDir          = getState $ head differences
        currentDiff         = head differences
        checkStateFailed    = checkState (stability-1) direction (ignoreLastDifference (tail differences) currentDiff)

ignoreLastDifference :: [Integer] -> Integer -> [Integer]
ignoreLastDifference differences lastdifference 
    | length differences == 0 = differences
    | length differences == 1 = [lastdifference +head differences]
    | otherwise               = (lastdifference +head differences) : tail differences

-- Sets the reactor direction to unknown for the first iteration
newCheckState :: [Integer] -> Bool
newCheckState = checkState 2 Unknown

-- Is the difference out of bounds?
isUnsafeDiff :: Integer -> Bool
isUnsafeDiff difference
    | difference >  3     = True
    | difference < -3     = True
    | difference == 0     = True
    | otherwise           = False

main = do
    -- Reading from the file
    handle <- openFile "2024_02_input" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let matrix      = map lineToInts x
    let diffmatrix  = map diffList matrix
    let safetys     = map newCheckState diffmatrix
    let safetysbackwards     = map newCheckState $ map reverse diffmatrix
    let allsafe = zipWith (||) safetys safetysbackwards
    let totalSafe   = sum $ map fromEnum allsafe
    putStr $ show totalSafe