{- |
Module      : Ourlude
Description : A collection of utility functions and operators for tinyHaskell to C compilation.
Copyright   : (c) Amr Farid Almorsi, 2024
License     : BSD3
Maintainer : amr.saleh@ejust.edu.eg
Stability   : experimental
Portability : POSIX

This module provides a set of utility functions and operators that are useful
in the context of transpiling tinyHaskell code to C code. It includes
composition operators and utility functions for working with monads and
either types.
-}

module Ourlude
 ( module Prelude,
    first, second,
    (<|),
    (|>),
    foldMapM,
    mapLeft,
    (<<<),
    (>>>),
 )
where

import Prelude
import Data.Bifunctor (first, second)



{- |
Transform an either by mapping on its left side.
This function is useful for applying a function to the left side of an Either value.

Example:

@
let e = Left 5
let f = mapLeft (+3) e
-- f is now Left 8
@
-}
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (f >>> Left) Right


{- |
Map over a list monadically, then squash the results monoidally.
This function is useful for applying a monadic action to each element of a list
and then combining the results using the monoid instance of the result type.

Example:

@
let xs = [1, 2, 3]
let ys = foldMapM (return . (+3)) xs
-- ys is now [4, 5, 6]
@
-}
foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

{- |
A left-to-right function composition operator.
It takes two functions and composes them in a left-to-right manner.

Example:

@
let f = (+3)
let g = (*2)
let h = f >>> g
-- h is now a function that adds 3 to its input and then multiplies by 2
@
-}
infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}


{- |
A right-to-left function composition operator.
It takes two functions and composes them in a right-to-left manner.

Example:

@
let f = (+3)
let g = (*2)
let h = g <<< f
-- h is now a function that adds 3 to its input and then multiplies by 2
@
-}
infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}



{- |
A backwards composition operator, which is an alternative to the ($) operator.
It takes a function and a value, applies the function to the value, and returns the result.

Example:

@
let x = 5
let y = (+3) <| x
-- y is now 8
@
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

{- |
A forward composition operator, which is an alternative to the (&) operator.
It takes a value and a function, applies the function to the value, and returns the result.

-}
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}