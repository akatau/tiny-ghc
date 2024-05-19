data ListOf a = Element a (ListOf a) | EmptyList

countFrom :: Int -> ListOf Int
countFrom n = Element n (countFrom (n + 1)) 
-- |          ^ This is an *infinite* list here.

at :: Int -> ListOf a -> a
at 0 (Element x _) = x
at n (Element _ xs) = at (n - 1) xs


main :: Int
main = at 100 (countFrom 0)
