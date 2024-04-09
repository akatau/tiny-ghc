{-# LANGUAGE FlexibleInstances #-}
-- | Language Extension: FlexibleInstances

-- This line enables the `FlexibleInstances` language extension within the current module.

-- The `FlexibleInstances` extension relaxes the default type instance resolution rules in tinyHaskell. 
-- Without this extension, type instance declarations must strictly follow the pattern:
-- 
-- instance SomeClass (Type a1 a2 a3) where
--   someFunction :: (Type a1 a2 a3) -> 


-- | Module: Types

-- This module defines the core types and functions related to representing and manipulating types in the language.
-- This line declares a tinyHaskell module named `Types`. The `where` clause following the module name
-- specifies the entities (functions, types, constants) that are exported from the module.

-- The exported entities are listed within parentheses:

-- - `Type (..)`: This expands all constructors of the `Type` data type. All functions defined for
--   manipulating `Type` values are also implicitly exported.
-- - `TypeArgument`: The type constructor for type variables.
-- - `TypeName`: The type constructor for type names.
-- - `FreeTypeVariables (..)`: This expands all functions and types within the `FreeTypeVariables` type class.
-- - `PolyType (..)`: This expands all constructors of the `PolyType` data type. All functions defined for
--   manipulating `PolyType` values are also implicitly exported.
-- - `makePolymorphic`: A function that takes a type expression and closes it over all free type names.
-- - `moreGeneral`: A function that compares the generality of two schemes.

-- This module defines the core types and functions related to representing and manipulating types in the
-- language. It provides data types for representing different kinds of types (basic types, function types,
-- custom types, type variables), schemes (types with bound variables), and a type class (`FreeTypeVariables`)
-- for extracting free type names from various type-related values. It also includes functions for closing
-- type expressions and comparing the generality of schemes.
module Types (
  -- | Type representation
  Type (..),

  -- | Type variable constructor
  TypeArgument,

  -- | Type name constructor
  TypeName,

  -- | Type class for extracting free type variables
  FreeTypeVariables (..),

  -- | PolyType type (type with bound variables)
  PolyType (..),

  -- | Function to close a type expression over free names
  makePolymorphic,

  -- | Function to compare generality of schemes
  moreGeneral
) where




import qualified Data.Set as Set
import Ourlude  -- Assuming this imports common types and functions

-- | Type Variable

-- | A type variable represents a placeholder within a type expression.
--   It is denoted as a lower-case string name.
type TypeArgument = String

-- | Type Identifier

-- | A type name represents a named type constructor.
--   It is denoted as an upper-case string name.
type TypeName = String

infixr 2 :->  -- Defines function type arrow with right associativity (e.g., A -> B -> C)

-- | Type Representation

-- | The `Type` data type represents different kinds of types in the language.
--   - `StringT`: The type of strings.
--   - `IntT`: The type of integers.
--   - `BoolT`: The type of booleans.
--   - `CustomType name arguments`: A custom type with a name (`name`) and a list of argument types (`arguments`).
--   - `TVar var`: A type variable with a name (`var`).
--   - `t1 :-> t2`: A function type where `t1` is the argument type and `t2` is the return type.
-- Our representation of a type
data Type
  = -- The type of strings
    StringT
  | -- The type of integers
    IntT
  | -- The type of booleans
    BoolT
  | -- A custom type, with a given name, and a list of arguments
    CustomType TypeName [Type]
  | -- A type variable
    TVar TypeArgument
  | -- A function type
    Type :-> Type
  deriving (Show, Eq)

-- | PolyType

-- | The `PolyType` data type represents a type quantified over a set of polymorphic type variables.
--   It stores the list of variables (`[TypeArgument]`) and the underlying type (`Type`).
data PolyType = PolyType [TypeArgument] Type deriving (Show, Eq)

-- | Free Type Variables

-- | The `FreeTypeVariables` type class defines a way to extract all the free type names
--   (unbound type variables) from a type or type-related value.
class FreeTypeVariables a where
  freeTypeVars :: a -> Set.Set TypeName  -- Extracts free type names from a value of type 'a'.

-- | Instance for `Type`

-- | Provides an implementation of `freeTypeVars` for the `Type` data type.
--   - Basic types (`StringT`, `IntT`, `BoolT`) have no free type names.
--   - `TVar a`: The free type name is the variable name itself.
--   - `t1 :-> t2`: Free type names are the union of free names in `t1` and `t2`.
--   - `CustomType _ ts`: Free type names are the union of free names in all argument types (`ts`).
instance FreeTypeVariables Type where
  freeTypeVars IntT = Set.empty
  freeTypeVars StringT = Set.empty
  freeTypeVars BoolT = Set.empty
  freeTypeVars (TVar a) = Set.singleton a
  freeTypeVars (t1 :-> t2) = freeTypeVars t1 <> freeTypeVars t2
  freeTypeVars (CustomType _ ts) = foldMap freeTypeVars ts  -- Combine free names from all arguments.

-- | Instance for `TypeName`

-- | Provides an implementation of `freeTypeVars` for the `TypeName` data type.
--   A type name itself is considered its only free type name.
instance FreeTypeVariables TypeName where
  freeTypeVars = Set.singleton

-- | Instance for `PolyType`

-- | Provides an implementation of `freeTypeVars` for the `PolyType` data type.
--   Free type names in a scheme are the difference between the free names in the
--   underlying type and the list of bound type variables.
instance FreeTypeVariables PolyType where
  freeTypeVars (PolyType vars t) = Set.difference (freeTypeVars t) (Set.fromList vars)  -- Remove bound variables.

-- | Instance for Sets

-- | Provides an implementation of `freeTypeVars` for sets of any type that supports `FreeTypeVariables`.
--   Free type names are the union of free names in all elements of the set.
instance FreeTypeVariables a => FreeTypeVariables (Set.Set a) where
  freeTypeVars = foldMap freeTypeVars  -- Combine free names from all elements in the set.

-- | Closing and Comparing Schemes

-- | **makePolymorphic**: Closing a Type Expression

-- This function, `makePolymorphic`, takes a type expression (`Type`) and closes it over all of the free type
-- names (unbound type variables) appearing within it. It creates a `PolyType` where the free type names
-- become bound type variables.

-- The process involves:
--
-- 1. Extracting free type names: It uses the `freeTypeVars` function from the `FreeTypeVariables` type class to
--    extract all the free type names from the given type expression.
-- 2. Converting to list: The extracted set of free type names is converted to a list using `Set.toList`.
-- 3. Constructing scheme: A `PolyType` is created with the list of free type variables as the first
--    argument and the original type expression as the second argument.

-- Essentially, `makePolymorphic` makes the type expression explicitly polymorphic by binding all its free type
-- variables.

makePolymorphic :: Type -> PolyType
makePolymorphic t = PolyType (freeTypeVars t |> Set.toList) t

-- | **moreGeneral**: Comparing Generality of Schemes

-- This function, `moreGeneral`, compares the generality of two schemes. It takes two `PolyType` arguments
-- and returns a `Bool` value.

-- A scheme `s1` is considered more general than `s2` if it has more bound type variables. This means
-- `s1` can potentially be instantiated to a wider range of types than `s2`.

-- The comparison is based on the length of the bound variable lists in the schemes. A scheme with a
-- longer list of bound variables is considered more general.

moreGeneral :: PolyType -> PolyType -> Bool
moreGeneral (PolyType vars1 _) (PolyType vars2 _) = length vars1 >= length vars2

