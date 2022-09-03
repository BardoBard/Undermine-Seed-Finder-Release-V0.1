module Binary.Shift where

import Basic ( pop )
import Binary.Basic ( toBinx )
import Binary.Conversion ( toDec, decToBase )
import Binary.Operators ()

-- |
-- shifts a signed binary string n places (in binary) to the left
--
-- e.g | shiftSB2BL32 5 "111011" -> "11101100000"
--
-- returns: shifted binary32 string
shiftBinL32 :: Int -> [Char] -> [Char]
shiftBinL32 n list = toBinx 32 $ shiftBinL n list;

-- |
-- shifts a signed binary string n places (in binary) to the left
--
-- e.g | shiftUB2BR32 8 "101010101000" -> "01010"
--
-- returns: shifted binary32 string
shiftBinR32 :: Int -> [Char] -> [Char]
shiftBinR32 n list = toBinx 32 $ shiftBinR n list;

-- |
-- shifts a signed binary string n places (in binary) to the left
--
-- e.g | shiftSB2BL32 5 "111011" -> "11101100000"
--
-- returns: shifted binary string
shiftBinL :: Int -> [Char] ->  [Char]
shiftBinL n list = list ++ replicate n '0'

-- |
-- shifts a signed binary string n places (in binary) to the left
--
-- e.g | shiftBinR 8 "101010101000" -> "01010"
--
-- returns: shifted binary string
shiftBinR :: Int -> [Char] ->  [Char]
shiftBinR n list = "0" ++ pop n list


--not using this
--you probably shouldn't either

shiftR :: Integral a => Int -> a -> String -> [Char]
shiftR z x y = take z (decToBase 2 x y) ++ replicate z '0'

shiftDecToBinL ::  Int -> Int -> [Char]
shiftDecToBinL z x =  decToBase 2 x $ replicate z '0'

shiftDecToBinR :: Integral a => Int -> a -> [Char]
shiftDecToBinR z x = shiftBinR z $ decToBase 2 x ""

shiftUDecR32 :: Integral a => Int -> a -> Int
shiftUDecR32 n dec = do
    let bin = toBinx 32 $ shiftDecToBinR n dec;
    if head bin == '1' then toDec bin - 4294967296 else toDec bin

shiftSDecL32 :: Int -> Int -> Int
shiftSDecL32 n dec = toDec $ toBinx 32 $ shiftDecToBinL n dec;
--for real D: