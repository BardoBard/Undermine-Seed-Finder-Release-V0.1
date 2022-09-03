module Binary.Logicgates where

import Binary.Basic ( matchLength )

-- NOT

-- | 
-- bitwise not
--
-- e.g | notB '1' -> '0'
--
-- returns: inverted bit
notB :: Char -> Char
notB x
        | x == '1'  = '0'
        | otherwise = '1'

-- | 
-- used for binary subtraction
--
-- same as notB
minB :: Char -> Char
minB = notB

--BUFFER

-- |
-- doesn't do anything
bufferB :: Char -> Char
bufferB x = x

-- | 
-- used for binary addition
--
-- same as a buffer
plusB :: Char -> Char
plusB = bufferB

-- ORs

-- | 
-- bitwise or
--
-- e.g | orB '1' '0' -> '1'
--
-- returns: char of 0 or 1
orB :: Char -> Char -> Char
orB x y
        | x == '1' || y == '1' = '1'
        | otherwise            = '0'

-- |
-- bitwise inverse of or
--
-- e.g | norB '1' '0' -> '0'
--
-- returns: char of 0 or 1
norB :: Char -> Char -> Char
norB x y = notB $ orB x y

-- |
-- bitwise xor
--
-- e.g | xorB '1' '1' -> '0'
--
-- returns: char 0 or 1
xorB :: Char -> Char -> Char
xorB x y
        | andB x y      == '1' = '0'
        | x == '1' || y == '1' = '1'
        | otherwise            = '0'

-- | 
-- extension of xorB
--
-- this will allow you to xor a string instead of a char
--
-- e.g | xorB' "101001" "10100" -> "111101"
--
-- returns: binary string of converted xor
xorB' :: [Char] -> [Char] -> [Char]
xorB' x y = zipWith xorB (matchLength y x) (matchLength x y)

-- |
-- inverse of xor
--
-- e.g | xnorB '1' '0' -> '0'
--
-- returns char of 0 or 1
xnorB :: Char -> Char -> Char
xnorB x y = notB $ xorB x y

-- ANDs

-- | 
-- bitwise and
--
-- e.g | andB '1' '1' -> '1'
--
-- returns: bit of 1 when both parameters are 1
andB :: Char -> Char -> Char
andB x y
        | x == '1' && y == '1' = '1'
        | otherwise            = '0'

-- |
-- inverse and
--
-- e.g | nandB '1' '0' -> '1'
nandB :: Char -> Char -> Char
nandB x y = notB $ andB x y

