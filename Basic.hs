module Basic where
import Control.Lens
import Data.Char

-- |
-- removes the last n elements from an array
--
-- e.g | pop 3 "10011"            -> "10"
--
-- e.g | pop 3 [1,3,5,6,7,8,9,10] -> [1,3,5,6,7]
--
-- returns: new array
pop :: Int -> [a] -> [a]
pop n list = take (length list - n) list

remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)

placeAt :: Int -> a -> [a] -> [a]
placeAt index value list
    | length list < index = list
    | otherwise           = take index list ++ value : drop index list

replace' :: [Int] -> [a] -> [a] -> [a]
replace' [] _ list  = list
replace' _ [] list  = list
replace' (i:index) (v:value) list = replace' index value $ replaceAt i v list

deleteAt :: Int -> [a] -> [a]
deleteAt index list
    | length list <= index = list
    | otherwise           = back ++ tail front
        where
            (back, front) = splitAt index list

move' [] _  list = list
move' _  [] list = list
move' (i:index) (p:place) list = move' index place (move i p list)

move :: Int -> Int -> [a] -> [a]
move index place list
    | length list <= index = list
    | otherwise            = placeAt place (list !! index) (deleteAt index list)


replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

owo111 [] _  list = list
owo111 _  [] list = list
owo111 (i:index) (v:value) list = owo111 index value (replaceAt i v list)


replaceAt'' index value list = list & element index .~ value

--this needs looking at
iterateFunc :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
iterateFunc 0 _ x = x
iterateFunc n f x = iterateFunc (n - 1) f (f x)

-- | 
-- used for undermine
--
-- e.g | badImul' 100 -> 181243325300
badImul' :: Int -> Int
badImul' = badImul 1812433253

-- | 
-- a bad way to do the javascript "imul"
--
-- e.g badImul 5 500 -> 2500
--
-- returns a multiplication of two @Uint32@
badImul :: Int -> Int -> Int
badImul x y = toUInt32 $ x * y

-- |
-- int to Uint32
--
-- returns: Uint32
toUInt32 :: Int -> Int
toUInt32 x = do
    let int32 = x `mod` 4294967295 - x `div` 4294967295 + 1
    if x > 4294967295
        then
            if int32 < 0
            then 4294967295 + int32 + 1
            else int32
        else x

-- |
-- int to Sint32
--
-- returns: Sint32
toInt32 :: Int -> Int
toInt32 x =
    if x > 2147483647
        then x - ceiling (fromIntegral x / 2147483647) * 2147483647
        else x

listToLower list = map toLower <$> list

-- | 
-- can't go smaller than 0
--
-- e.g | nateralPos -10 -> 0
--
-- e.g | nateralPos 100 -> 100
--
-- returns: positive integer or 0
naturalPos :: (Ord p, Num p) => p -> p
naturalPos x
    | x <= 0    = 0
    | otherwise = x