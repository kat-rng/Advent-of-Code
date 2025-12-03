import Math.NumberTheory.Logarithms

-- Finds repetition in integers
isRepeatedDiv :: Integer -> Integer -> Bool
isRepeatedDiv x slices
    | size `mod` slices /= 0 = False
    | slices /= 2 && mid == upper = isRepeatedDiv lower (slices-1)
    | slices /= 2 = False
    | upper == lower = True
    | otherwise = False
    where 
        size   = fromIntegral (1 + integerLog10 x)
        slicer = (10 ^ (size - size `div` slices))
        slicerMid = (10 ^ (size - 2 * size `div` slices))
        upper = x `div` slicer
        lower = x `mod` slicer
        mid   = lower `div` slicerMid



