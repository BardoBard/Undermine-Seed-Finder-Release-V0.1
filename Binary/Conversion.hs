module Binary.Conversion where
  
import Data.List ( foldl' )
import Data.Char ( digitToInt, intToDigit )
import Binary.Operators ( binPlus )

-- |
-- converts binary to an int
--
-- e.g | toDec "11010" -> 26
--
-- returns: converted integer
toDec :: [Char] -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- |
-- this converts a binary string to two's complement
-- it's pretty useful if you want to know what value a binary string has but for math it's better to not touch it
-- 
-- e.g |         toTwos "10001101" -> "001110011"
--
-- e.g | toDec $ toTwos "10001101" ->     115
--
-- returns: two's complement string from 1's complement
toTwos :: [Char] -> [Char]
toTwos x = binPlus "1" $ inverseBin x

-- | 
-- inverses all of the 1 and 0's
--
-- e.g | inverseBin "1110010" -> "0001101"
--
-- returns: inverted binary
inverseBin :: [Char] -> [Char]
inverseBin = map $ \bit -> if bit =='0' then '1'; else '0'

-- |
-- converts an integer to a base value (2 for binary, 16 for hex)
--
-- e.g | decToBase 2 141 "" -> "10001101"
--
-- returns: converted binary string
decToBase :: Integral a => a -> a -> ShowS
decToBase base = showIntAtBase base intToDigit

-- |
-- I stole this from some package, because I couldn't import it
showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= 1 = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to unsupported base " ++ show (toInteger base))
  | n0 <  0   = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to negative number " ++ show (toInteger n0))
  | otherwise = showIt (quotRem n0 base) r0
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
        _ -> showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d)
      r' = c : r
