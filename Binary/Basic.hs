module Binary.Basic where
import Basic ( naturalPos )

-- | 
-- convers a binary string/[char] to the length of x
--
-- e.g | toBinx 2 "11010" -> "10"
--
-- returns: new binary string/[char]
toBinx :: Int -> [Char] -> [Char]
toBinx n bin = drop (naturalPos (length bin - n)) bin

-- | 
-- matches the length of x to y and returns y
--
-- e.g | matchLength "100" "1" -> "001"
--
-- returns: new binary string/[char] of y
matchLength :: [Char] -> [Char] -> [Char]
matchLength x y = replicate (naturalPos (length x - length y)) '0' ++ y

-- | 
-- checks which binary string is bigger
--
-- if you don't know the length of your binary string/[char] use -> biggestBinMatched
--
-- e.g | biggestBin "110" "011" -> True
--
-- returns: true if x > y
biggestBin :: [Char] -> [Char] -> Bool
biggestBin [] _ = True
biggestBin _ [] = False
biggestBin (x:xs) (y:ys) 
                | x /= y    = x == '1' 
                | otherwise = biggestBin xs ys

-- | 
-- checks which matched binary string is bigger
--
-- e.g | biggestBin "0000000000010" "011" -> "011"
--
-- returns: biggest binary string/[char]
biggestBinMatched :: [Char] -> [Char] -> [Char]
biggestBinMatched x y 
                | biggestBin x' y' = x'
                | otherwise               = y'
              where 
                  x' = matchLength y x
                  y' = matchLength x y

-- | 
-- checks which matched binary string is smaller
--
-- e.g | biggestBin "11001" "0111" -> "0111"
--
-- returns: smallest binary string/[char]
smallestBinMatched :: [Char] -> [Char] -> [Char]
smallestBinMatched x y
                | biggestBin x' y' = y'
                | otherwise        = x'
              where 
                  x' = matchLength y x
                  y' = matchLength x y