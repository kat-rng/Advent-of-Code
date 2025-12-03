import Data.List
import System.IO

maxJolt x = maximum $ sort x

pt1 = do
    -- Reading from the file
    handle <- openFile "2025_03_input" ReadMode
    contents <- hGetContents handle
    let lins = lines contents
    
    let maxFirstJolt = map (\x -> maxJolt $ init x) lins


    putStr $ show maxFirstJolt
