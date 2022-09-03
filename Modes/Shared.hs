module Modes.Shared where

--my libs

import Basic ( replaceAt, iterateFunc, badImul', naturalPos )
import Binary.Shift ( shiftBinL32, shiftBinR32 )
import Binary.Logicgates ( xorB' )
import Binary.Conversion ( toDec, decToBase )


--this works every time
range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min

nextUInt seed =
    toDec (xorB' x' y')
    where
        seedBin = decToBase 2 seed ""
        x     = xorB' seedBin $ shiftBinL32 11 seedBin --add if '1' then two else continue
        x'    = xorB' x       $ shiftBinR32 8 x
        y     = decToBase 2   (iterateFunc  3 badImul' seed) ""
        y'    = xorB' y       $ shiftBinR32 19 y

reduceWeight :: [Int] -> Int -> Int
reduceWeight [] _ = 0
reduceWeight (x:xs) y
                | (y - x) <= 0 = 0
                | otherwise    = 1 + reduceWeight xs (y - x)

sum' x y z
            | x == (-1) = 0
            | otherwise = z !! naturalPos y


getIndex seed items sum = reduceWeight items $ range 1 sum seed

replaceAt' :: (Eq a1, Num a1, Num a2) => a1 -> Int -> [a2] -> [a2]
replaceAt' x x2 list
    | x == (-1) = list
    | otherwise = replaceAt x2 0 list