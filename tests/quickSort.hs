data ListOf a = Element a (ListOf a) | EmptyList

toList x = Element x EmptyList

type String' = ListOf String

type ListOfChar = String'

digitToString x = case x of
                  0 -> "0"
                  1 -> "1"
                  2 -> "2"
                  3 -> "3"
                  4 -> "4"
                  5 -> "5"
                  6 -> "6"
                  7 -> "7"
                  8 -> "8"
                  9 -> "9"

append :: a -> ListOf a -> ListOf a
append a EmptyList = toList a
append a (Element x xs) = Element x (append a xs)

concat :: ListOf a -> ListOf a -> ListOf a
concat EmptyList ys = ys
concat (Element x xs) ys = Element x (concat xs ys)

map :: (a -> b) -> ListOf a -> ListOf b
map f EmptyList = EmptyList
map f (Element x xs) = Element (f x) (map f xs)

foldr :: (a -> b -> b) -> b -> ListOf a -> b
foldr f init EmptyList = init
foldr f init (Element x xs) = f x (foldr f init xs)

mod :: Int -> Int -> Int
mod x y = x - (x / y) * y
mod' x y = mod y x

div x y = x / y
div' x y = div y x 

showListOfString :: ListOf String -> String
showListOfString EmptyList = "[]"
showListOfString (Element x xs) = x ++ " : " ++ showListOfString xs

range :: Int -> ListOf Int
range 0 = EmptyList
range x = append x (range (x-1))

reversedRange :: Int -> ListOf Int
reversedRange 0 = EmptyList
reversedRange x = Element x (range (x-1))

countDigits :: Int -> Int
countDigits 0 = 0
countDigits x = 1 + countDigits (x / 10)

iterate :: (a -> a) -> a -> Int -> ListOf a
iterate f x 0 = EmptyList
iterate f x n = Element (f x) (iterate f (f x) (n-1))

reverseList :: ListOf a -> ListOf a
reverseList EmptyList = EmptyList
reverseList (Element x xs) = append x (reverseList xs)

stringConcat x y = x ++ y

powers x = iterate (div' 10) (x*10) (countDigits x)
showInt x = foldr stringConcat "" (reverseList $ map (digitToString) (map (mod' 10) (powers x)))

showListOfInt :: ListOf Int -> String
showListOfInt EmptyList = "[]"
showListOfInt (Element x xs) = (showInt x) ++ " : " ++ (showListOfInt xs)

filterLessThan :: Int -> ListOf Int -> ListOf Int
filterLessThan _ EmptyList = EmptyList
filterLessThan pivot (Element x xs) =
  if x < pivot then Element x (filterLessThan pivot xs)
  else filterLessThan pivot xs

filterGreaterOrEqual :: Int -> ListOf Int -> ListOf Int
filterGreaterOrEqual _ EmptyList = EmptyList
filterGreaterOrEqual pivot (Element x xs) =
  if x >= pivot then Element x (filterGreaterOrEqual pivot xs)
  else filterGreaterOrEqual pivot xs

quickSort :: ListOf Int -> ListOf Int
quickSort EmptyList = EmptyList
quickSort (Element x xs) =
  let less = filterLessThan x xs
      greaterOrEqual = filterGreaterOrEqual x xs
  in concat (quickSort less) (Element x (quickSort greaterOrEqual))


main :: String
main = showListOfInt $ quickSort $ reverseList $ range 1000
-- main = showListOfInt (quickSort unorderedInts)
--        where
--           unorderedInts = reversedRange 24 -- Numbers from 1 to 80 in descending order.


