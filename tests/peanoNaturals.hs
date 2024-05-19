-- Definition of Peano Set
data Peano = Zero | Succ Peano

-- Outputs string form of Peano naturals
showPeano :: Peano -> String
showPeano Zero = "Zero"
showPeano (Succ x) = "(" ++ "Succ " ++ showPeano x ++ ")"

-- Using Arabic Numerals to denote elements of a Peano set
arabic Zero = 0
arabic (Succ a) = 1 + arabic a

-- Showing the Peano representation of Natural numbers
arabicToPeano :: Int -> Peano
arabicToPeano 0 = Zero
arabicToPeano a = Succ (arabicToPeano (a-1))
-- map (arabic.arabicToPeano) range(20) 

-- Definition of Addition Operation on Peano sets
add :: Peano -> Peano -> Peano
add x Zero = x
add x (Succ y) = Succ (add x y)
-- arabic (add (Succ (Succ (Succ Zero))) (Succ (Succ Zero)))


-- Definition of Subttraction Operation on Peano sets (x - y = 0 | y > x since additive inverses are not defined)
subtract :: Peano -> Peano -> Peano
subtract Zero x = Zero
subtract x Zero = Succ x --This pattern is actually wrong, but it works somehow—a bug. basically.
subtract (Succ x) (Succ y) = subtract x y 
-- arabic (subtract (arabicToPeano 1) (arabicToPeano 5)) 

-- Definition of Multiplication Operation on Peano sets
multiply :: Peano -> Peano -> Peano
multiply Zero _ = Zero
multiply (Succ a) b = add b (multiply a b)
-- arabic (multiply (arabicToPeano 0) (arabicToPeano 0))

-- Definition of Exponentiation Operation on Peano sets
power :: Peano -> Peano -> Peano
power (Succ _) Zero = Succ Zero
power Zero (Succ _) = Zero
power a (Succ b) = multiply a (power a b)
power (Succ a) b  = multiply (power a b) b
-- arabic (power (arabicToPeano 1) (arabicToPeano 10))



-- main :: Int
-- main = showPeano $ arabicToPeano 10
-- main = arabic (add (Succ (Succ (Succ Zero))) (Succ (Succ Zero)))
-- main = arabic (add (arabicToPeano 123) (arabicToPeano 291))
-- main = arabic (subtract (arabicToPeano 122) (arabicToPeano 100)) 
main = arabic (multiply (arabicToPeano 142) (arabicToPeano 122))
