module Binary.Operators where 

import Binary.Logicgates ( minB, plusB, orB, xorB, andB )
import Binary.Basic ( matchLength )

-- |
-- calculates the difference of an adder
--
-- e.g | difference '1' '1' '1' -> '1'
--
-- returns: char of 0 or 1
difference :: Char -> Char -> Char -> Char
difference x y = xorB $ xorB x y

-- | 
-- calculates the borrow or carry of an adder/subtractor bit/char
--
-- e.g | borrowOrCarry '1' '1' '0' minB  -> '0'
--
-- e.g | borrowOrCarry '1' '1' '0' plusB -> '1'
--
-- returns: char of 0 or 1
borrowOrCarry :: Char -> Char -> Char -> (Char -> Char) -> Char
borrowOrCarry x y bin f = orB (andB (f (xorB x y)) bin) (andB (f x) y)

-- | 
-- calculates the borrow or carry of an adder/subtractor binary string and matches them
--
-- e.g | borrowOrCarryBin "111001" "11011" minB  -> "011110"
--
-- e.g | borrowOrCarryBin "111001" "11011" plusB -> "111011"
--
-- returns: binary string of borrow or carry
borrowOrCarryBin :: [Char] -> [Char] -> (Char -> Char) -> [Char]
borrowOrCarryBin x y = borrowOrCarryBin' (matchLength y x) (matchLength x y) '0'

-- |
-- for general use look towards borrowOrCarryBin
-- 
-- calculates the borrow or carry of an adder/subtractor binary string
--
-- e.g | borrowOrCarryBin' "111001" "11011" '0' minB  -> "11110"
--
-- e.g | borrowOrCarryBin' "111001" "11011" '0' plusB -> "11011"
--
-- returns: binary string of borrow or carry
borrowOrCarryBin' :: [Char] -> [Char] -> Char -> (Char -> Char) -> [Char]
borrowOrCarryBin' _  []  _  _  = []
borrowOrCarryBin' []  _  _  _  = []
borrowOrCarryBin' x y b f = borrowOrCarryBin' (init x)  (init y) (borrowOrCarry (last x) (last y) b f) f ++ [borrowOrCarry (last x) (last y) b f]

-- |
-- this is a template for binMin and BinPlus
adder :: (Char -> Char) -> [Char] -> [Char] -> [Char]
adder f x y = reverse $ zipWith3 difference (reverse ('0' : matchLength x y) ) (reverse ('0' : matchLength y x)) (reverse (borrowOrCarryBin x y f ++ "0"))

-- | 
-- subtracts one set of binary strings from the other
--
-- e.g | binMin "1" "1001" -> "11001" (use toTwos to turn this into two's complement)
--
-- e.g | binMin "1001" "1" -> "01000"
--
-- returns: resulting binary string
binMin :: [Char] -> [Char] -> [Char]
binMin = adder minB

-- | 
-- adds one set of binary strings to the other
--
-- e.g | binPlus "1" "1001" -> "01010"
--
-- returns: resulting binary string
binPlus :: [Char] -> [Char] -> [Char]
binPlus = adder plusB