data ListOf a = Element a (ListOf a) | EmptyList

at :: Int -> ListOf a -> a
at 0 (Element x _) = x
at n (Element _ xs) = at (n - 1) xs

tail :: ListOf a -> ListOf a
tail EmptyList = EmptyList
tail (Element _ rest) = rest

-- An infinite—yep, not a typo—list of all Fibanocci numbers
fibsMemo :: ListOf Int
fibsMemo = 
  let 
    add :: ListOf Int -> ListOf Int -> ListOf Int
    add (Element a as) (Element b bs) = Element (a + b) (add as bs)
  in Element 0 (Element 1 (add fibsMemo (tail fibsMemo))) 

fib :: Int -> Int
fib n = at n fibsMemo

main :: Int
main = fib 30 --30th Fibonacci number should be 832040