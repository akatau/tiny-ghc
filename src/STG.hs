{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | This module defines the Spineless Tagless G-machine (STG) representation.
--
-- The STG representation is an intermediate form generated from simplified expressions
-- (defined in the `ASTSimplifier` module) during the compilation process. It serves as a
-- low-level foundation for further code generation or analysis.

module STG (
  -- * Data Types
  STG(..),             -- The main STG program representation
  LambdaBinding(..),          -- Bindings for names and lambda expressions
  BoxedValueType(..),          -- Types for boxed values
  BuiltinFunction(..),          -- Built-in operations
  Atom(..),             -- Basic data units used in expressions
  Value(..),          -- Value values (integers, strings)
  ValueIdentifier,              -- Type for names used in expressions
  LambdaExpression(..),        -- Representation of lambda expressions
  SimplifiedExpression(..),             -- Expressions in the STG language
  ConstructorTag,                  -- Integer type representing tags for alternatives
  CaseBranches(..),             -- Branches of shallow alternatives in case expressions
  PrimitiveValue(..),        -- PrimitiveValue values like integers and strings
  UpdateableFlag(..),        -- Type class for updatable values

  -- * Functions
  compileToSTG,                  -- Main function for STG compilation
) where

-- -- | **Previously defined modules:**
-- --
-- -- * `ASTSimplifier`: Defines the simplified expression representation used as input for STG compilation.

-- -- * Data Types

-- -- | The `STG` type represents a complete STG program. It consists of:
-- --
-- -- * `bindings`: A list of top-level bindings for names and lambda expressions.
-- -- * `entry`: The name of the entry point function in the program.
-- data STG = STG {
--   bindings :: [LambdaBinding],
--   entry :: ValueIdentifier
-- }
-- | Imports necessary modules for STG compilation.

import Control.Applicative (liftA2)  -- For lifting operations to the monad stack

import Control.Monad.Reader  -- For using an environment during compilation

import Data.List (find)  -- For finding elements in lists

import qualified Data.Set as Set  -- For working with sets of variable names

import Ourlude  -- Custom prelude module (details assumed known)

import Control.Monad.State


import ASTSimplifier
  (
    AST (..),
    ConstructorName,
    Value (..),
    ValueIdentifier,
    ValueDefinition (..),
  )

-- | Imports simplified expression types and functions from the `ASTSimplifier` module.
--
-- The `ASTSimplifier` module defines the simplified expression representation used as input
-- for STG compilation. Functions like `AST`, `Value`, `ValueIdentifier`, etc., are used to
-- access and manipulate these simplified expressions.

import qualified ASTSimplifier as S

import Types (PolyType (..), Type (..))  -- For type information (assumed defined elsewhere)
-- | Imports type information (`PolyType` and `Type`) from the `Types` module.
--
-- Type information might be used for further analysis or code generation based on the STG representation.

type STGMEnv = Set.Set ValueIdentifier  -- Environment type: set of free variable names

type STGState = (STGMEnv, [LambdaBinding])  -- Compiler state: environment and accumulated bindings


-- | Represents an actual primitive value used within STG expressions.
data PrimitiveValue
  = -- | A primitive integer value.
    PrimInt Int
  | -- | A primitive string value.
    PrimString String
  deriving (Show, Eq)


-- | PrimitiveValue Built-in Operations

-- | Represents primitive built-in operations available within STG expressions.
--
-- These built-in operations differ from those defined in the `ASTSimplifier` module as they
-- operate *exclusively* on primitive values (`PrimitiveValue` data type). This allows for
-- simpler compilation compared to handling more complex user-defined functions.

data BuiltinFunction
  = Add
  | Sub
  | Mul
  | Div
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | EqualTo
  | NotEqualTo
  | Negate
  | Concat
  | ExitWithInt
  | ExitWithString
  deriving (Show, Eq)

-- | Basic Data Unit (Atom)

-- | Represents a unit of data that is considered simple enough to be used directly
-- within STG expressions.
--
-- In the context of STG, expressions are restricted to a simpler form compared to
-- the `ASTSimplifier` module. This limitation (using `Atom` as the building block) aims to
-- make compilation of these expressions significantly easier.

data Atom
  = -- | A literal value wrapped in the `PrimitiveValue` data type
    PrimitiveAtom PrimitiveValue
  | -- | A reference to a name using the `ValueIdentifier` type
    NameAtom ValueIdentifier
  deriving (Show, Eq)

-- | ConstructorTag for Algebraic Data Types

-- | Represents a tag used to identify different alternatives within an algebraic data type.
--
-- This integer type serves as a label for each branch (constructor) of an algebraic
-- data type when it's used within STG expressions.
type ConstructorTag = Int


-- | Expressions in the STG Language

-- | Represents an expression within the STG language.
--
-- This data type defines various forms of expressions that can be used to build STG programs.
-- Each constructor corresponds to a specific kind of expression:
--
-- * `PrimitiveValue`: A literal value wrapped in the `PrimitiveValue` data type.
-- * `Apply`: Applying a function (identified by a `ValueIdentifier`) to a list of `Atom`s (arguments).
-- * `Error`: Introducing a panic with an associated error message string. 
--     This is used for handling cases where no matching branch is found in a `Case` expression.
-- * `Constructor`: Applying an algebraic data type (ADT) constructor (identified by a `ConstructorTag`) to a list of `Atom`s (arguments).
--     The constructor must be fully saturated (have all its arguments provided).
-- * `Box`: Boxing an `Atom` into a heap value with a specific `BoxedValueType`.
-- * `BuiltinFunction`: Applying a primitive built-in operation (defined in the `BuiltinFunction` data type) to a list of `Atom`s (arguments).
--     The built-in operation must be fully saturated (have all its arguments provided).
-- * `Case`: A case expression for handling different alternatives based on the result of evaluating another expression (the scrutinee).
--     It takes the scrutinee expression, a list of variable names for pattern matching, and a list of `CaseBranches` (alternative branches).
-- * `Let`: A sequence of bindings (name-expression pairs) followed by an expression.
--     Bindings introduce new names and their corresponding definitions before evaluating the final expression.
data SimplifiedExpression
  = PrimitiveValue PrimitiveValue
  | Apply ValueIdentifier [Atom]
  | Error String
  | Constructor ConstructorTag [Atom]
  | Box BoxedValueType Atom
  | BuiltinFunction BuiltinFunction [Atom]
  | Case SimplifiedExpression [ValueIdentifier] CaseBranches
  | Let [LambdaBinding] SimplifiedExpression
  deriving (Show, Eq)

-- | Types for Boxed Values

-- | Represents the type of a boxed value in the STG language.
--
-- This data type defines two possible types for boxed values:
--
-- * `IntBox`: A boxed integer value.
-- * `StringBox`: A boxed string value.
data BoxedValueType
  = IntBox
  | StringBox
  deriving (Show, Eq)

-- | Branches of Shallow Alternatives in Case Expressions

-- | Represents different branches of shallow alternatives within a `Case` expression.
--
-- Unlike patterns, alternatives only consider a single level of data structure. Each
-- alternative branch can potentially match a specific value and provide a corresponding
-- expression to evaluate. Additionally, all alternatives include a default expression
-- to handle cases where none of the specific branches match.
--
-- Splitting alternatives based on literal types (integer, string) simplifies code
-- generation. A single alternative for all literals would lose type information, making
-- code generation more complex.

data CaseBranches
  = -- | Branches for boxed integer literals and a default expression (Maybe SimplifiedExpression)
    IntegerCases [(Int, SimplifiedExpression)] (Maybe SimplifiedExpression)
  | -- | Branches for boxed string literals and a default expression (Maybe SimplifiedExpression)
    StringCases [(String, SimplifiedExpression)] (Maybe SimplifiedExpression)
  | -- | Bind a primitive value based on the type of the boxed value (BoxedValueType)
    --   and introduce a new name (ValueIdentifier) with its corresponding expression (SimplifiedExpression)
    BindPrim BoxedValueType ValueIdentifier SimplifiedExpression
  | -- | Unbox a primitive value from a specific box type (BoxedValueType) and introduce
    --   a new name (ValueIdentifier) with its corresponding expression (SimplifiedExpression).
    --
    -- This unboxing is primarily intended for use with built-in functions. The name
    -- introduced cannot be used like a normal variable name for pattern matching.
    -- For pattern matching against integers or strings, use `IntegerCases` and `StringCases`
    -- which handle unboxing if necessary.
      Unbox BoxedValueType ValueIdentifier SimplifiedExpression
  | -- | Branches for constructor tags, introducing names for arguments,
    --   and concluding with a default expression (Maybe SimplifiedExpression)
    ConstructorAlternatives [((ConstructorTag, [ValueIdentifier]), SimplifiedExpression)] (Maybe SimplifiedExpression)
  deriving (Show, Eq)

-- | UpdateableFlag Thunks

-- | Represents a flag indicating whether a thunk (unevaluated expression) can be updated.
--
-- By default, all thunks are considered updateable. Marking a thunk as non-updateable is
-- an optimization technique for specific cases where the thunk is known to be fully
-- evaluated or used only once. This information can be used during code generation.
data UpdateableFlag = N  -- Not updateable
               | U  -- UpdateableFlag
  deriving (Show, Eq)

-- | Lambda Expressions

-- | Represents a lambda expression within the STG language.
--
-- A lambda expression defines a function by binding a set of variables to its body expression.
-- This data type captures the essential components of a lambda form:
--
-- * `freeVars`: A list of variable names (represented by `ValueIdentifier`) that are free in the body expression
--               but not formally defined as parameters of the lambda.
-- * `updateable`: A flag of type `UpdateableFlag` indicating whether the lambda expression can be updated
--                 (evaluated multiple times). By default, lambdas are updateable, but this flag can be
--                 used for optimizations in specific cases.
-- * `params`: A list of variable names representing the formal parameters of the lambda expression.
-- * `body`: The expression that defines the function's computation based on the provided parameters.
data LambdaExpression = LambdaExpression [ValueIdentifier] UpdateableFlag [ValueIdentifier] SimplifiedExpression
  deriving (Show, Eq)

-- | Class for Computing Free Variable Names

-- | The `FreeNames` class provides a mechanism to compute the set of free variable names
-- within a given expression or data structure. A free variable is a variable that is used
-- within the expression but not formally defined or bound within its scope.

class FreeNames a where
  -- | Computes the set of free variable names within a value of type `a`.
  freeNames :: a -> Set.Set ValueIdentifier

-- | Instance for Lists

-- | Inherits the `FreeNames` constraint for lists.
--
-- The free variables of a list are the union of free variables in each element of the list.
instance FreeNames a => FreeNames [a] where
  freeNames = foldMap freeNames

-- | Instance for Maybe

-- | Inherits the `FreeNames` constraint for the `Maybe` type constructor.
--
-- The free variables of `Maybe a` are the free variables within the contained value (if any).
instance FreeNames a => FreeNames (Maybe a) where
  freeNames Nothing = mempty  -- Empty set for Nothing
  freeNames (Just a) = freeNames a  -- Free variables of the contained value

-- | Instance for Atoms

-- | Inherits the `FreeNames` constraint for the `Atom` data type.
--
-- The only free variable in a `NameAtom` is the variable name itself. Other atoms have no free variables.
instance FreeNames Atom where
  freeNames (NameAtom n) = Set.singleton n  -- Set containing only the variable name
  freeNames _ = mempty                     -- Empty set for other atoms

-- | Instance for Expressions

-- | Inherits the `FreeNames` constraint for the `SimplifiedExpression` data type.
--
-- The free variables in an expression are computed by considering different constructors:
--   * `Apply`: The name of the applied function and the free variables in the arguments.
--   * `Constructor`: Free variables within the arguments.
--   * `BuiltinFunction`: Free variables within the arguments.
--   * `Case`: Free variables in the scrutinee expression and the alternatives.
--   * `Let`: Free variables in the body expression after considering bound names in the bindings.
--   * Other constructors: No free variables.
instance FreeNames SimplifiedExpression where
  freeNames (Apply name atoms) = Set.singleton name <> freeNames atoms
  freeNames (Constructor _ atoms) = freeNames atoms
  freeNames (BuiltinFunction _ atoms) = freeNames atoms
  freeNames (Case e _ alts) = freeNames e <> freeNames alts
  freeNames (Let bindings e) =
    let (names, insideBindings) = foldMap (\(LambdaBinding name e') -> (Set.singleton name, freeNames e')) bindings
     in Set.difference (freeNames e <> insideBindings) names
  freeNames _ = mempty

-- | Instance for Alternatives

-- | Inherits the `FreeNames` constraint for the `CaseBranches` data type.
--
-- The free variables in alternatives depend on the specific constructor:
--   * `IntegerCases`, `StringCases`: Free variables in expressions within branches and the default expression.
--   * `ConstructorAlternatives`: Free variables in the body expressions of branches (excluding bound names).
--   * `BindPrim`, `Unbox`: Free variables in the body expression after removing the bound name.
instance FreeNames CaseBranches where
  freeNames (IntegerCases alts e) = freeNames e <> freeNames (map snd alts)
  freeNames (StringCases alts e) = freeNames e <> freeNames (map snd alts)
  freeNames (ConstructorAlternatives alts e) =
    let inAlts = foldMap (\((_, names), e') -> Set.difference (freeNames e') (Set.fromList names)) alts
     in freeNames e <> inAlts
  freeNames (BindPrim _ n e) = Set.delete n (freeNames e)
  freeNames (Unbox _ n e) = Set.delete n (freeNames e)

-- | Instance for Lambda Expressions

-- | Inherits the `FreeNames` constraint for the `LambdaExpression` data type.
--
-- The free variables in a lambda expression are the variables used in the body expression
-- that are not included in the list of formal parameters.
instance FreeNames LambdaExpression where
  freeNames (LambdaExpression _ _ names e) = Set.difference (freeNames e) (Set.fromList names)

-- | Bindings in STG Programs

-- | Represents a binding within an STG program.
--
-- A binding associates a variable name (`ValueIdentifier`) with a lambda expression (`LambdaExpression`).
-- In STG, bindings always involve lambda forms, reflecting the connection to heap-allocated thunks
-- (unevaluated expressions).
data LambdaBinding = LambdaBinding ValueIdentifier LambdaExpression
  deriving (Show, Eq)

-- | STG Programs

-- | Represents an entire STG program.
--
-- An STG program is defined by two components:
--
-- * `bindings`: A list of top-level bindings, where each binding associates a variable name
--               with a lambda expression.
-- * `entryPoint`: The entry point of the program, represented as a lambda expression.
data STG = STG [LambdaBinding] LambdaExpression
  deriving (Show, Eq)

-- | STG Compilation Errors

-- | Represents potential errors that can occur during STG generation.
--
-- This data type defines two possible error types:
--
-- * `NoEntryPoint`: Indicates that the STG program does not have a defined entry point.
-- * `IncorrectEntryPointType PolyType`: Signals that the entry point (lambda expression) has an
--   incompatible type scheme.
data STGError
  = NoEntryPoint
  | IncorrectEntryPointType PolyType
  deriving (Show, Eq)

-- | Information for STG Compilation

-- | Represents the information available during STG compilation.
--
-- This data type captures the context used for compiling expressions into STG:
--
-- * `topLevelNames`: A set containing all variable names defined at the top level of the program.
-- * `constructorInfo`: Information about constructors (their types and arities) retrieved from
--   the `S.ConstructorMap` type (assumed to be defined in the `ASTSimplifier` module).
data CompilationInfo = CompilationInfo
  { topLevelNames :: Set.Set ValueIdentifier  -- Set of top-level variable names
  , constructorInfo :: S.ConstructorMap  -- Information about constructors
  }

-- | Context for STG Code Generation

-- | Represents the context (`STGContext`) in which STG code is generated.
--
-- This newtype utilizes the `ReaderT`, `State`, and `Monad` transformers to build a layered context
-- for STG code generation. It provides access to the following information and functionalities:
--
-- * `ReaderT CompilationInfo`: Access to global information about the program being compiled, stored in the
--   `CompilationInfo` type. This information includes the set of top-level variable names and constructor details.
-- * `State Int`: Management of a counter using the `State` monad. This counter can be used for various
--   purposes during code generation, such as generating unique generateFreshName variable names.
-- * `Monad`: Provides basic monadic functionality for sequencing computations within the STG generation process.
-- * `MonadState Int`: Extends the monad with the ability to manipulate an internal state (the counter) using
--   functions like `get`, `put`, and `modify`.
-- * `MonadReader CompilationInfo`: Allows reading the `CompilationInfo` data structure containing global program information.
-- * `Functor`, `Applicative`: Provides basic functional programming features for working with values within
--   the `STGContext` context.
newtype STGContext a = STGContext (ReaderT CompilationInfo (State Int) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadReader CompilationInfo)

-- | Accessing Constructor Information within STGContext

-- | Inherits the `S.HasVariantInfo` constraint for the `STGContext` monad transformer.
--
-- This instance allows direct access to the constructor information (`S.ConstructorMap`) within the
-- `STGContext` context using the `asks` function from the `MonadReader` typeclass. The constructor information
-- is assumed to be part of the `CompilationInfo` data type.
instance S.HasVariantInfo STGContext where
  constructorMap = asks constructorInfo

-- | Running STG Code Generation

-- | Runs the STG code generation computation within a specific context (`CompilationInfo`).
--
-- This function takes an `STGContext` computation (wrapped in the `STGContext` monad transformer) and an `CompilationInfo`
-- data structure containing global program information. It then executes the computation using the
-- transformers stacked within `STGContext`:
--
-- 1. `runReaderT info`: Runs the computation with access to the provided `CompilationInfo` using the `ReaderT`
--    transformer.
-- 2. `runState 0`: Runs the computation starting with an initial state of 0 using the `State` transformer.
--    This state can be used to generate generateFreshName variable names.
-- 3. `fst`: Extracts the final result from the computation after running the transformers.
runSTGM :: STGContext a -> CompilationInfo -> a
runSTGM (STGContext m) info =
  m |> (`runReaderT` info) |> (`runState` 0) |> fst

-- | Generate a Fresh Variable Identifier

-- | Generates a generateFreshName variable name within the `STGContext` context.
--
-- This function utilizes the `State` monad transformer to manage a counter for generating unique names.
-- It performs the following steps:
--
-- 1. `get`: Retrieves the current value of the counter.
-- 2. `put (x + 1)`: Updates the counter by incrementing its value.
-- 3. `return ("$$" ++ show x)`: Returns a new variable name constructed by prefixing "$$" with
--    the current counter value (converted to a string using `show`).
generateFreshName :: STGContext ValueIdentifier
generateFreshName = do
  x <- get
  put (x + 1)
  return ("$$" ++ show x)

-- | Lookup Constructor ConstructorTag

-- | Retrieves the tag associated with a constructor name within the `STGContext` context.
--
-- This function leverages the `S.HasVariantInfo` constraint to access the constructor information
-- stored in the `CompilationInfo` data structure. It performs the following:
--
-- 1. `S.getVariantInfo name`: Looks up the constructor with the given name using the
--    `S.getVariantInfo` function (assumed to be defined in the `ASTSimplifier` module). This
--    function is expected to raise an exception if the constructor is not found.
-- 2. `fmap S.constructorNumber`: Maps the retrieved constructor information using `fmap` to extract
--    the constructor tag (`S.constructorNumber`) from the result.
constructorTag :: ConstructorName -> STGContext ConstructorTag
constructorTag name = S.getVariantInfo name |> fmap S.constructorNumber

-- | Get Free Variable Names

-- | Computes the set of free variable names within a value in the `STGContext` context.
--
-- This function takes advantage of the `FreeNames` typeclass constraint and the `asks` function
-- from `MonadReader` to achieve the following:
--
-- 1. `asks (topLevelNames >>> Set.difference (freeNames a) >>> Set.toList)`:
--     - `asks`: Retrieves the `CompilationInfo` data structure using `asks`.
--     - `topLevelNames`: Accesses the set of top-level variable names from `CompilationInfo`.
--     - `Set.difference (freeNames a)`: Computes the difference between the free variables in the
--       provided value (`a`) and the set of top-level variable names. This ensures that top-level
--       variables are not considered free.
--     - `Set.toList`: Converts the resulting set of free variable names to a list.
computeFreeVariables :: FreeNames a => a -> STGContext [ValueIdentifier]
computeFreeVariables a =
  asks (topLevelNames >>> Set.difference (freeNames a) >>> Set.toList)


-- | Construct a Lambda Expression

-- | Creates a lambda expression within the `STGContext` context.
--
-- This function takes a list of formal parameter names (`names`) and an expression (`expr`) representing
-- the lambda body. It performs the following steps:
--
-- 1. `computeFreeVariables (LambdaExpression [] N names expr)`: Computes the free variables within a temporary lambda
--    form with the provided parameters, no updateable flag, and the given body expression. This helps
--    determine which variables are free within the lambda's scope.
-- 2. `updateable names expr`: Determines whether the resulting lambda expression can be updated (evaluated
--    multiple times) based on the parameter names and the body expression.
-- 3. `LambdaExpression free u names expr`: Constructs the final lambda expression with the computed free variables
--    (`free`), updateable flag (`u`), parameter names (`names`), and body expression (`expr`).
constructLambda :: [ValueIdentifier] -> SimplifiedExpression -> STGContext LambdaExpression
constructLambda names expr = do
  free <- computeFreeVariables (LambdaExpression [] N names expr)
  let u = updateable names expr
  return (LambdaExpression free u names expr)
  where
    updateable :: [ValueIdentifier] -> SimplifiedExpression -> UpdateableFlag
    -- If we have arguments, then we can't be evaluated further (N)
    updateable (_ : _) _ = N
    -- Primitives can't be evaluated any further (N)
    updateable [] (PrimitiveValue _) = N
    -- Errors and constructors are not updateable (N) since they exit or are fixed values
    updateable [] (Error _) = N
    updateable [] (Constructor _ _) = N
    updateable [] (Box _ _) = N
    -- Otherwise, the lambda is updateable (U)
    updateable _ _ = U

-- | Construct a Case Expression

-- | Creates a case expression within the `STGContext` context.
--
-- This function takes a scrutinee expression (`scrut`) and a list of alternatives (`alts`) and returns a
-- case expression. It performs the following step:
--
-- 1. `computeFreeVariables alts`: Computes the free variables within the list of alternatives.
-- 2. `Case scrut free alts`: Constructs the final case expression with the scrutinee expression, computed
--    free variables, and the list of alternatives.
buildCaseExpression :: SimplifiedExpression -> CaseBranches -> STGContext SimplifiedExpression
buildCaseExpression scrut alts = do
  free <- computeFreeVariables alts
  return (Case scrut free alts)

-- | Gather Applications in an Expression

-- | Extracts all applications (function calls) from an expression scheme.
--
-- This function recursively traverses the expression scheme (`expression`) and accumulates all encountered
-- function applications (`S.FunctionApplicationExpression`) into a list. It performs a depth-first search:
--
-- * `go (S.FunctionApplicationExpression f e) acc`: If the current node is an application, it adds the function (`f`) to the
--   accumulator (`acc`) and continues searching in the argument expression (`e`).
-- * `go e acc`: If the current node is not an application, it simply returns the expression itself
--   as the scheme and the accumulated applications so far.
extractFunctionCalls :: S.SimplifiedExpression PolyType -> (S.SimplifiedExpression PolyType, [S.SimplifiedExpression PolyType])
extractFunctionCalls expression = go expression []
  where
    go (S.FunctionApplicationExpression f e) acc = go f (e : acc)
    go e acc = (e, acc)

-- | Convert Literals to Atoms

-- | Converts a literal value (`Value`) into a pair consisting of bindings and an atom within the `STGContext` context.
--
-- This function handles different literal types:
--
-- * `StringLiteral s`:
--     1. Generates a generateFreshName variable name (`name`) using `generateFreshName`.
--     2. Creates a lambda expression (`l`) with an empty parameter list and a body expression that boxes
--        the string literal using `Box StringBox` and the `PrimitiveAtom (PrimString s)` constructor.
--     3. Returns a pair containing a binding with the generated name and the lambda expression, and a
--        `NameAtom` using the generateFreshName name.
-- * `IntegerLiteral i`: Similar to `StringLiteral`, but boxes the integer literal using `IntBox` and
--   `PrimInt`.
-- * `BooleanLiteral b`: Constructs a constructor atom representing the boolean value. It uses a constructor
--   with the value 1 for true and 0 for false.
convertLiteralToAtom :: Value -> STGContext ([LambdaBinding], Atom)
convertLiteralToAtom (StringLiteral s) = do
  name <- generateFreshName
  l <- constructLambda [] (Box StringBox (PrimitiveAtom (PrimString s)))
  return ([LambdaBinding name l], NameAtom name)
convertLiteralToAtom (IntegerLiteral i) = do
  name <- generateFreshName
  l <- constructLambda [] (Box IntBox (PrimitiveAtom (PrimInt i)))
  return ([LambdaBinding name l], NameAtom name)
convertLiteralToAtom (BooleanLiteral b) = do
  name <- generateFreshName
  l <- constructLambda [] (Constructor (if b then 1 else 0) [])
  return ([LambdaBinding name l], NameAtom name)

-- | Convert Expression PolyType to Atom

-- | Converts an expression scheme (`S.SimplifiedExpression PolyType`) into a pair consisting of bindings and an atom within
-- the `STGContext` context.
--
-- This function performs different actions depending on the type of expression scheme:
--
-- * `S.LiteralExpression l`: Delegates the conversion to `convertLiteralToAtom`.
-- * `S.IdentifierExpr n`:
--     1. Checks if the name (`n`) refers to a constructor using `S.hasVariant`.
--     2. If it's not a constructor, simply returns an empty list of bindings and a `NameAtom` with the name.
--     3. If it's a constructor, calls the `constructorToAtom` function (assumed to be defined
--        elsewhere) to handle converting the constructor to an atom.
-- * Other expression schemes (`e`):
--     1. Generates a generateFreshName variable name (`name`).
--     2. Converts the expression scheme to a lambda expression using `expressionToLambda`.
--     3. Removes the binding introduced by the generateFreshName variable name from the lambda expression using
--        `removeBinding name l`.
--     4. Returns a pair containing an empty list of bindings (since bindings are removed) and a
--        `NameAtom` with the generateFreshName name.
atomize :: S.SimplifiedExpression PolyType -> STGContext ([LambdaBinding], Atom)
atomize expression = case expression of
  S.LiteralExpression l -> convertLiteralToAtom l
  S.IdentifierExpr n -> do
    wasConstructor <- S.hasVariant n
    if not wasConstructor
      then return ([], NameAtom n)
      else constructorToAtom n
  e -> do
    name <- generateFreshName
    l <- expressionToLambda e
    return ([LambdaBinding name (removeBinding name l)], NameAtom name)

-- | Convert Atom to Expression

-- | Converts an atom (`Atom`) into an expression.
--
-- This function utilizes pattern matching to handle different atom types:
--
-- * `PrimitiveAtom l`: Returns a primitive expression using the primitive literal (`l`).
-- * `NameAtom n`: Returns an application expression with the atom's name and an empty argument list.
atomToExpr :: Atom -> SimplifiedExpression
atomToExpr (PrimitiveAtom l) = PrimitiveValue l
atomToExpr (NameAtom n) = Apply n []

-- | Construct a Let Expression

-- | Creates a `Let` expression within the STG language.
--
-- This function takes a list of bindings (`bindings`) and an expression (`e`) representing the body of
-- the `Let` expression. It performs the following:
--
-- * If the list of bindings is empty (`[]`), it simply returns the body expression (`e`) as the entire
--   `Let` expression has no bindings to define.
-- * If there are bindings, it constructs a `Let` expression with the provided bindings and the body expression.
makeLet :: [LambdaBinding] -> SimplifiedExpression -> SimplifiedExpression
makeLet [] e = e
makeLet bindings e = Let bindings e

-- | Result of Constructor Saturation

-- | Represents the possible outcomes of saturating a constructor with atoms.
--
-- This data type (`ConstructorResult`) captures two scenarios:
--
-- * `AlreadyFull tag atoms`: Indicates that the constructor already has the required number of arguments
--   (arity) and is filled with the provided atoms. It includes the constructor tag and the filled atom list.
-- * `NeededFilling bindings valueName`: Signals that the constructor needs more arguments to be filled.
--   It provides the bindings required to introduce generateFreshName variables for the missing arguments, and the name
--   of a variable representing the partially filled constructor.
data ConstructorResult = AlreadyFull ConstructorTag [Atom] | NeededFilling [LambdaBinding] ValueIdentifier

-- | Saturate a Constructor with Atoms

-- | Fills a constructor with atoms within the `STGContext` context.
--
-- This function takes three arguments:
--
-- * `alwaysSaturate`: A boolean flag indicating whether to always attempt saturation, even if the constructor
--   already has the correct number of arguments.
-- * `name`: The name of the constructor.
-- * `atoms`: A list of atoms provided as arguments to the constructor.
--
-- It performs the following steps:
--
-- 1. `(S.ConstructorInfo arity _ tag) <- S.getVariantInfo name`: Looks up the constructor information
--    using `S.getVariantInfo` (assumed from the `ASTSimplifier` module). This retrieves details like
--    arity (number of arguments) and tag.
-- 2. `let diff = arity - length atoms`: Calculates the difference between the constructor's arity and the
--    number of provided atoms.
-- 3. Conditional logic based on `alwaysSaturate` and `diff`:
--     - If `not alwaysSaturate` and `diff == 0` (constructor already has enough arguments), it returns
--       `AlreadyFull tag atoms`.
--     - Otherwise, it generates generateFreshName variable names using `replicateM diff generateFreshName` for the missing arguments.
-- 4. Constructs a partially filled constructor atom (`root`) with the tag, provided atoms, and generateFreshName variable
--    names.
-- 5. Generates a generateFreshName variable name (`bindingName`) for the entire partially filled constructor expression.
-- 6. Creates a lambda expression (`lambda`) that represents the application of the constructor (with placeholders
--    for missing arguments) to itself. The lambda expression binds the generateFreshName variable names introduced earlier.
-- 7. Returns `NeededFilling [LambdaBinding bindingName (removeBinding bindingName lambda)] bindingName`, which
--    contains the bindings for the generateFreshName variables, and the name of the variable representing the partially
--    filled constructor.
createConstructor :: Bool -> ConstructorName -> [Atom] -> STGContext ConstructorResult
createConstructor alwaysSaturate name atoms = do
  (S.ConstructorInfo arity _ tag) <- S.getVariantInfo name
  let diff = arity - length atoms
  if not alwaysSaturate && diff == 0
    then return (AlreadyFull tag atoms)
    else do
      lambdaNames <- replicateM diff generateFreshName
      let root = Constructor tag (atoms ++ map NameAtom lambdaNames)
      bindingName <- generateFreshName
      lambda <- constructLambda lambdaNames root
      return (NeededFilling [LambdaBinding bindingName (removeBinding bindingName lambda)] bindingName)

-- | Saturate Constructor and Convert to Expression

-- | Fills a constructor with atoms and converts the result to an expression within the `STGContext` context.
--
-- This function takes a constructor name (`name`), a list of atoms (`atoms`), and a list of bindings
-- (`bindings`) and performs the following:
--
-- 1. `convert <$>  createConstructor False name atoms`: Calls ` createConstructor` with
--    `alwaysSaturate` set to `False` to attempt saturation without forcing it. The result is wrapped in the
--    `convert` function (defined below).
-- 2. The `convert` function performs pattern matching on the `ConstructorResult`:
--     - `AlreadyFull tag as`: Constructs a `Let` expression with the provided bindings and a constructor
--       expression using the tag and the filled atom list (`as`). This case indicates that the constructor
--       already had the correct number of arguments or was forced to be filled even if it didn't require
--       all the provided atoms.
--     - `NeededFilling newBindings n`: Constructs a `Let` expression that combines the provided bindings
--       with the newly generated bindings (`newBindings`) to introduce generateFreshName variables for missing arguments.
--       It then applies the variable representing the partially filled constructor (`n`) with no arguments.
--       This case signifies that the constructor needed more arguments and generateFreshName variables were introduced
--       as placeholders for the missing ones.
constructorToExpression :: ConstructorName -> [Atom] -> [LambdaBinding] -> STGContext SimplifiedExpression
constructorToExpression name atoms bindings =
  convert <$>  createConstructor False name atoms
  where
    convert (AlreadyFull tag as) = makeLet bindings (Constructor tag as)
    convert (NeededFilling newBindings n) = makeLet (bindings <> newBindings) (Apply n [])

-- | Saturate Constructor and Convert to Atom

-- | Fills a constructor with atoms and converts the result to an atom within the `STGContext` context.
--
-- This function takes a constructor name (`name`) and performs the following:
--
-- 1. `(\(NeededFilling b n) -> (b, NameAtom n)) <$>  createConstructor True name []`: Calls
--    ` createConstructor` with `alwaysSaturate` set to `True` to force saturation even if the constructor
--    might already have the correct number of arguments. The result is wrapped in a function that extracts
--    the bindings and the name of the variable representing the partially filled constructor from the
--    `NeededFilling` case.
--
-- This function effectively converts a constructor to an atom, potentially introducing generateFreshName variables for
-- missing arguments. It's useful when the constructor needs to be treated as a single entity even if it's
-- not fully saturated.
constructorToAtom :: ConstructorName -> STGContext ([LambdaBinding], Atom)
constructorToAtom name =
  (\(NeededFilling b n) -> (b, NameAtom n)) <$>  createConstructor True name []

-- | Convert BuiltinFunction to Variable Identifier

-- | Converts a built-in function (`S.BuiltinFunction`) to a corresponding variable name within the STG language.
--
-- This function utilizes a pattern matching approach to map each built-in function to a specific string
-- representing its name in the STG language. The resulting string is used as a variable name.
--
-- Built-in functions and their corresponding variable names:
--
-- * `S.Add`: Translated to "$add"
-- * `S.Sub`: Translated to "$sub"
-- * `S.Mul`: Translated to "$mul"
-- * `S.Div`: Translated to "$div"
-- * `S.Concat`: Translated to "$concat" ( for string concatenation)
-- * `S.Less`: Translated to "$less" ( for comparison)
-- * `S.LessEqual`: Translated to "$less_equal" ( for comparison)
-- * `S.Greater`: Translated to "$greater" ( for comparison)
-- * `S.GreaterEqual`: Translated to "$greater_equal" ( for comparison)
-- * `S.EqualTo`: Translated to "$equal" ( for equality comparison)
-- * `S.NotEqualTo`: Translated to "$not_equal" ( for inequality comparison)
-- * `S.Or`: Translated to "$or" ( for logical OR)
-- * `S.And`: Translated to "$and" ( for logical AND)
-- * `S.Compose`: Translated to "$compose" ( for function composition)
-- * `S.Cash`: Translated to "$cash" (purpose depends on the specific system)
-- * `S.Negate`: Translated to "$neg" ( for negation)
builtinToVariable :: S.BuiltinFunction -> ValueIdentifier
builtinToVariable = \case
  S.Add -> "$add"
  S.Sub -> "$sub"
  S.Mul -> "$mul"
  S.Div -> "$div"
  S.Concat -> "$concat"
  S.Less -> "$less"
  S.LessEqual -> "$less_equal"
  S.Greater -> "$greater"
  S.GreaterEqual -> "$greater_equal"
  S.EqualTo -> "$equal"
  S.NotEqualTo -> "$not_equal"
  S.Or -> "$or"
  S.And -> "$and"
  S.Compose -> "$compose"
  S.Cash -> "$cash"
  S.Negate -> "$neg"

-- | Convert an Expression PolyType to STG Expression

-- | Converts an expression scheme (`S.SimplifiedExpression PolyType`) from the simplifier to an expression (`SimplifiedExpression`) within the
-- STG language. This function performs a comprehensive transformation, handling various expression types and
-- recursively converting nested expressions.
--
-- The conversion process follows these steps:
--
-- 1. `extractFunctionCalls >>>`: Applies the `extractFunctionCalls` function (assumed to be defined elsewhere)
--    to the expression scheme. This function extracts all function applications (applications of built-in
--    functions or constructors) and their arguments. The result is a pair of the expression itself (`e`) and
--    a list of argument expressions (`[]` if there are no function applications).
-- 2. Conditional branching based on the presence of function applications:
--     - `(e, [])`: If there are no function applications (`[]`), the `handle` function (defined below) is
--       called to handle the expression scheme itself (`e`).
--     - `(f, arguments)`: If there are function applications (`arguments`), a case statement checks the type of the
--       function (`f`):
--       - `S.BuiltinFunction b`: Built-in functions are treated as operators and assumed to be already fully saturated
--         during parsing. This case gathers atoms from the arguments (`arguments`) using `gatherAtoms`, and constructs
--         a `Let` expression with bindings from argument conversion and an application of the built-in function's
--         corresponding variable name (`builtinToVariable b`) to the gathered atoms.
--       - `S.IdentifierExpr n`: Handles expressions that refer to a name (`n`). It gathers atoms from arguments (`arguments`)
--         and checks if the name refers to a constructor using `S.hasVariant`.
--         - If it's not a constructor, a `Let` expression is created with bindings from argument conversion and
--           an application of the name (`n`) to the gathered atoms.
--         - If it's a constructor, `constructorToExpression` is used to potentially fill the constructor with
--           generateFreshName variables for missing arguments and convert the result to a `Let` expression.
--       - Other expression types (`e`): An error is thrown indicating that only built-in functions and
--         names are expected as function applications in this context.
--
-- The `handle` function defined below deals with various expression scheme types individually.
expressionToSTG :: S.SimplifiedExpression PolyType -> STGContext SimplifiedExpression
expressionToSTG =
  extractFunctionCalls >>> \case
    (e, []) -> handle e
    (f, arguments) -> case f of
      S.BuiltinFunction b -> do
        (bindings, atoms) <- gatherAtoms arguments
        return (makeLet bindings (Apply (builtinToVariable b) atoms))
      S.IdentifierExpr n -> do
        (bindings, atoms) <- gatherAtoms arguments
        wasConstructor <- S.hasVariant n
        if not wasConstructor
          then return (makeLet bindings (Apply n atoms))
          else constructorToExpression n atoms bindings
      e -> do
        (argBindings, atoms) <- gatherAtoms arguments
        (eBindings, atom) <- atomize e
        return <| case atom of
          PrimitiveAtom _ -> error "Primitives cannot be functions"
          NameAtom n -> makeLet (argBindings ++ eBindings) (Apply n atoms)
  where
    -- | Helper function to handle various expression scheme types
    handle :: S.SimplifiedExpression PolyType -> STGContext SimplifiedExpression
    handle (S.LiteralExpression l) =
      return <| case l of
        StringLiteral s -> Box StringBox (PrimitiveAtom (PrimString s))
        IntegerLiteral i -> Box IntBox (PrimitiveAtom (PrimInt i))
        BooleanLiteral b -> Constructor (if b then 1 else 0) []
    handle (S.IdentifierExpr n) = do
      wasConstructor <- S.hasVariant n
      if not wasConstructor
        then return (Apply n [])
        else constructorToExpression n [] []
    handle (S.Error s) = return (Error s)
    handle (S.LetExpression defs e) = do
      defs' <- definitionsToBindings defs
      e' <- expressionToSTG e
      return (makeLet defs' e')
    handle lambda@S.LambdaExpression {} = do
      (bindings, atom) <- atomize lambda
      return (makeLet bindings (atomToExpr atom))
    handle S.FunctionApplicationExpression {} = error "Apply Expressions shouldn't appear here"
    handle S.BuiltinFunction {} = error "Unary builtin operation"
    handle (S.CaseExpression _ []) = return (Error "Empty Case Expression")
    handle (S.CaseExpression e branches) = expressionToSTG e >>= convertCaseBranches branches
    -- | Gather bindings and atoms from a list of expressions
    gatherAtoms :: [S.SimplifiedExpression PolyType] -> STGContext ([LambdaBinding], [Atom])
    gatherAtoms = mapM atomize >>> fmap gatherBindings
    -- | Combine bindings from a list of bindings and atoms
    gatherBindings :: [([b], a)] -> ([b], [a])
    gatherBindings l = (concatMap fst l, map snd l)


-- | Convert Case Expression Branches

-- | Converts a list of case expression branches (`[(S.Match, S.SimplifiedExpression PolyType)]`) and a scrutinee expression
-- (`scrut`) to an STG expression using the `STGContext` monad. This function handles matching different pattern types
-- against the scrutinee and generates the corresponding case expression branches.
--
-- The conversion process works as follows:
--
-- 1. Match matching on the first branch from the list:
--     - `S.LiteralPattern (S.IntegerLiteral _)`:
--       - Extracts integer literals from all branches using `findPatterns`.
--       - Finds the default expression branch using `findDefaultExpr`.
--       - Constructs a case expression with integer alternatives (`IntegerCases`) using the extracted literals
--         and the default expression.
--     - Similar logic applies for `S.LiteralPattern (S.BooleanLiteral _)` and `S.LiteralPattern (S.StringLiteral _)`,
--       extracting boolean literals, string literals, and constructing case expressions with constructor
--       alternatives (`ConstructorAlternatives`) or string alternatives (`StringCases`) respectively.
--     - `S.ConstructorPattern _ _`:
--       - Extracts constructor tags from branches with constructor patterns using `findPatterns`.
--       - Finds the default expression branch using `findDefaultExpr`.
--       - Constructs a case expression with constructor alternatives (`ConstructorAlternatives`) using the extracted tags
--         and the default expression.
--     - `S.Wildcard`: Converts the expression (`e`) associated with the wildcard pattern directly.
--
-- 2. Helper functions:
--     - `findDefaultExpr`: Finds the branch with a wildcard pattern (`S.Wildcard`) and extracts its
--       associated expression using `traverse (snd >>> expressionToSTG)`.
--     - `findPatterns`: Filters branches based on a predicate applied to the pattern (`conv`), extracts the
--       desired value from matching patterns using the provided conversion function, and combines them with
--       the converted expressions for each branch using `liftA2 (,)`. It stops processing branches
--       once a wildcard pattern is encountered (`takeWhile (fst >>> (/= S.Wildcard))`).
convertCaseBranches :: [(S.Match, S.SimplifiedExpression PolyType)] -> SimplifiedExpression -> STGContext SimplifiedExpression
convertCaseBranches branches scrut = case head branches of
  (S.LiteralPattern (S.IntegerLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.IntegerLiteral i)) -> return i) branches
    default' <- findDefaultExpr branches
    buildCaseExpression scrut (IntegerCases branches' default')
  (S.LiteralPattern (S.BooleanLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.BooleanLiteral b)) -> return b) branches
    default' <- findDefaultExpr branches
    let constrBranches = map (first (\b -> (if b then 1 else 0, []))) branches'
    buildCaseExpression scrut (ConstructorAlternatives constrBranches default')
  (S.LiteralPattern (S.StringLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.StringLiteral s)) -> return s) branches
    default' <- findDefaultExpr branches
    buildCaseExpression scrut (StringCases branches' default')
  (S.ConstructorPattern _ _, _) -> do
    branches' <- findPatterns (\(S.ConstructorPattern cstr names) -> (,names) <$> constructorTag cstr) branches
    default' <- findDefaultExpr branches
    buildCaseExpression scrut (ConstructorAlternatives branches' default')
  (S.Wildcard, e) -> expressionToSTG e
  where
    -- | Find the default expression branch (with wildcard pattern)
    findDefaultExpr :: [(S.Match, S.SimplifiedExpression PolyType)] -> STGContext (Maybe SimplifiedExpression)
    findDefaultExpr = find (fst >>> (== S.Wildcard)) >>> traverse (snd >>> expressionToSTG)

    -- | Find branches matching a predicate on the pattern and extract values
    findPatterns :: (S.Match -> STGContext a) -> [(S.Match, S.SimplifiedExpression PolyType)] -> STGContext [(a, SimplifiedExpression)]
    findPatterns conv = takeWhile (fst >>> (/= S.Wildcard)) >>> traverse (\(pat, e) -> liftA2 (,) (conv pat) (expressionToSTG e))

-- | Remove a LambdaBinding Identifier from a Lambda Form

-- | Removes a specific variable name (`name`) from the free variables (`free`) of a lambda form (`LambdaExpression`).
-- This function is useful when a variable introduced during lambda expression conversion needs to be eliminated
-- from the free variables list to avoid naming conflicts.
--
-- It performs filtering on the `free` variables, keeping only those that don't match the provided `name`.
removeBinding :: ValueIdentifier -> LambdaExpression -> LambdaExpression
removeBinding name (LambdaExpression free u names expr) =
  LambdaExpression (filter (/= name) free) u names expr

-- | Convert an Expression to a Lambda Form

-- | Converts an expression scheme (`S.SimplifiedExpression PolyType`) to a lambda form (`LambdaExpression`) within the `STGContext` monad.
-- This function handles various expression scheme types and ensures the result is always a lambda form, even if
-- the original expression wasn't a lambda. In cases where the input expression isn't a lambda, an empty list of
-- arguments is used.
--
-- The conversion process follows these steps:
--
-- 1. `gatherLambdas`: This helper function identifies nested lambda expressions and accumulates their variable names.
--     - For a `S.LambdaExpression`:
--       - Recursively calls `gatherLambdas` on the inner expression (`e`) to gather its lambdas and variable names.
--       - Prepends the current lambda's name (`name`) to the accumulated names.
--     - For other expression types:
--       - Returns an empty list of names and the original expression.
-- 2. Convert the remaining expression (`e`) using `expressionToSTG`.
-- 3. Construct a `LambdaExpression` using the gathered variable names (`names`), the converted expression (`e'`),
--    and an empty list of explicit arguments (`u`).
expressionToLambda :: S.SimplifiedExpression PolyType -> STGContext LambdaExpression
expressionToLambda expr = do
  let (names, e) = gatherLambdas expr
  e' <- expressionToSTG e
  constructLambda names e'
  where
    gatherLambdas :: S.SimplifiedExpression PolyType -> ([ValueIdentifier], S.SimplifiedExpression PolyType)
    gatherLambdas (S.LambdaExpression name _ e) =
      let (names, e') = gatherLambdas e
       in (name : names, e')
    gatherLambdas e = ([], e)

-- | Convert a Value Definition to an STG LambdaBinding

-- | Converts a value definition scheme (`ValueDefinition PolyType`) to an STG binding (`LambdaBinding`) within the
-- `STGContext` monad. This function handles definitions that bind a name to an expression.
--
-- The conversion process involves these steps:
--
-- 1. Extract the name (`name`) from the definition.
-- 2. Convert the expression (`e`) associated with the definition to a lambda form using `expressionToLambda`.
-- 3. Apply `removeBinding name` to the resulting lambda form to remove the name itself from the free
--    variables list. This is necessary to avoid naming conflicts when the bound expression contains references
--    to the same name.
-- 4. Construct a binding using the extracted name (`name`) and the modified lambda form.
definitionToBinding :: ValueDefinition PolyType -> STGContext LambdaBinding
definitionToBinding (ValueDefinition name _ _ e) =
  LambdaBinding name <<< removeBinding name <$> expressionToLambda e

-- | Convert Value Definitions to STG Bindings

-- | Converts a list of value definition schemes (`[ValueDefinition PolyType]`) to a list of STG bindings
-- (`[LambdaBinding]`) within the `STGContext` monad. This function iterates over each definition in the list and calls
-- `definitionToBinding` to convert it to an STG binding. The resulting list of bindings represents the variable
-- definitions from the program.
definitionsToBindings :: [ValueDefinition PolyType] -> STGContext [LambdaBinding]
definitionsToBindings = mapM definitionToBinding

-- | Convert an AST PolyType to STG

-- | Converts an abstract syntax tree scheme (`AST PolyType`) to an STG program (`Either STGError STG`) within
-- the `STGContext` monad. This function performs the main conversion from the source program representation to the
-- STG representation.
--
-- The conversion process focuses on finding the main entry point and ensuring it has the correct type. It then
-- gathers information about all top-level definitions and builds the STG program structure.
--
-- Steps involved:
--
-- 1. Check for a definition named "main":
--     - If "main" is not found, return an error (`Left NoEntryPoint`).
--     - If "main" is found:
--       - Verify its type. It should be either `IntT` (for integer return) or `StringT` (for string return).
--         - For valid types, proceed with conversion.
--         - For invalid types, return an error (`Left IncorrectEntryPointType`).
-- 2. Gather bindings from all definitions using `definitionsToBindings`.
-- 3. Construct the main entry point function using `makeEntry`:
--     - Create a case expression that applies the "main" function with no arguments and unpacks the result
--       using the appropriate box type (either `IntBox` or `StringBox`). This unpacks the returned value from
--       the "main" function.
--     - Wrap the case expression in a lambda form to define the entry point function.
-- 4. Combine built-in function names with the bindings from definitions using `fmap (builtinBindings ++)`.
-- 5. Construct the STG program using `STG`:
--     - Pass the gathered bindings (`bindings`).
--     - Pass the constructed entry point function (`entry`).
astToSTG :: AST PolyType -> STGContext (Either STGError STG)
astToSTG (AST _ defs) =
  find (\(S.ValueDefinition n _ _ _) -> n == "main") defs |> \case
    Nothing -> return (Left NoEntryPoint)
    Just (S.ValueDefinition n _ (PolyType [] IntT) _) -> do
      bindings <- gatherBindings
      entry <- makeEntry IntBox ExitWithInt n
      return (Right (STG bindings entry))
    Just (S.ValueDefinition n _ (PolyType [] StringT) _) -> do
      bindings <- gatherBindings
      entry <- makeEntry StringBox ExitWithString n
      return (Right (STG bindings entry))
    Just (S.ValueDefinition _ _ s _) -> return (Left (IncorrectEntryPointType s))
  where
    makeEntry boxType b n = do
      theCase <- buildCaseExpression (Apply n []) (Unbox boxType "#v" (BuiltinFunction b [NameAtom "#v"]))
      return (LambdaExpression [] N [] theCase)

    gatherBindings =
      defs |> definitionsToBindings |> fmap (builtinBindings ++)

-- | Gather Information from the AST

-- | Gathers information from an abstract syntax tree scheme (`AST PolyType`) and returns an `CompilationInfo` record
-- within the `STGContext` monad. This function collects information about built-in function names and top-level
-- definitions.
--
-- The information is used for various purposes during code generation, such as type checking and code
-- optimization.
--
-- Steps involved:
--
-- 1. Extract all top-level definition names using `gatherTopLevel`.
-- 2. Combine built-in function names with the top-level definition names.
-- 3. Construct an `STGInfo` record with the gathered information and the original AST information.
extractASTInfo :: AST PolyType -> CompilationInfo
extractASTInfo (AST info defs) =
  let topLevel = gatherTopLevel defs |> Set.fromList
   in CompilationInfo (builtinFunctions <> topLevel) info
  where
    gatherTopLevel :: [S.ValueDefinition t] -> [ValueIdentifier]
    gatherTopLevel = map (\(S.ValueDefinition n _ _ _) -> n)

-- | Built-in Function Names

-- | A set containing the names of all built-in functions (`Set.Set ValueIdentifier`). This set is pre-populated
-- with function names defined elsewhere ( in a module providing built-in function definitions).
--
-- The set of built-in function names is used during various stages of code generation, such as converting
-- expressions and performing type checking. By having a set of known built-in names, the compiler can distinguish
-- between user-defined functions and built-in functions.
builtinFunctions :: Set.Set ValueIdentifier
builtinFunctions = builtinBindings |> map (\(LambdaBinding n _) -> n) |> Set.fromList


-- | Built-in Function Bindings

-- | A list of bindings (`[LambdaBinding]`) representing built-in functions. This list provides the definitions for
-- the core set of built-in functions supported by the STG language.
--
-- Each binding has a name (`$name`) and an associated lambda form (`LambdaExpression`) that defines the function's
-- behavior. The lambda form can reference other built-in functions using their names.
--
-- This list is used during various stages of code generation, such as converting expressions and performing
-- type checking. By having definitions for built-in functions, the compiler can translate their usage in the
-- source program to equivalent STG expressions.

builtinBindings :: [LambdaBinding]
builtinBindings =
  [ LambdaBinding "$add" (unboxedIntBuiltin Add),
    LambdaBinding "$sub" (unboxedIntBuiltin Sub),
    LambdaBinding "$mul" (unboxedIntBuiltin Mul),
    LambdaBinding "$div" (unboxedIntBuiltin Div),
    LambdaBinding "$concat" (unboxedStringBuiltin Concat),
    LambdaBinding "$less" (boolBuiltin Less),
    LambdaBinding "$less_equal" (boolBuiltin LessEqual),
    LambdaBinding "$greater" (boolBuiltin Greater),
    LambdaBinding "$greater_equal" (boolBuiltin GreaterEqual),
    LambdaBinding "$equal" (boolBuiltin EqualTo),
    LambdaBinding "$not_equal" (boolBuiltin NotEqualTo),
    LambdaBinding
      "$or"
      ( LambdaExpression
          []
          N
          ["$0", "$1"]
          ( Case
              (Apply "$0" [])
              ["$1"]
              ( ConstructorAlternatives
                  [ ((1, []), Constructor 1 []),
                    ((0, []), Apply "$1" [])
                  ]
                  Nothing
              )
          )
      ),
    LambdaBinding
    "$and"
    ( LambdaExpression
        []
        N
        ["$0", "$1"]
        ( Case
            (Apply "$0" [])
            ["$1"]
            ( ConstructorAlternatives
                [ ((0, []), Constructor 0 []),
                    ((1, []), Apply "$1" [])
                ]
                Nothing
            )
        )
    ),
    LambdaBinding "$cash" (LambdaExpression [] N ["$0", "$1"] (Apply "$0" [NameAtom "$1"])),
    LambdaBinding
      "$neg"
      ( LambdaExpression
          []
          N
          ["$0"]
          ( Case
              (Apply "$0" [])
              []
              ( Unbox IntBox "#0" (makeIntBox (BuiltinFunction Negate [NameAtom "#0"]))
              )
          )
      ),
    LambdaBinding
    "$compose"
    ( LambdaExpression
        []
        N
        ["$0", "$1", "$2"]
        ( Let
            [ LambdaBinding
                "$3"
                ( LambdaExpression ["$1", "$2"] U [] (Apply "$1" [NameAtom "$2"])
                    )
            ]
            (Apply "$0" [NameAtom "$3"])
        )
    ) 
  ]
    -- | Helper functions for creating built-in function bindings with unpacking and packing

    -- | This function creates a `LambdaExpression` representing a built-in function with unpacking and packing capabilities.
    --
    -- It takes three arguments:
    --
    -- * `boxType`: The box type (`BoxedValueType`) used for arguments and the result of the built-in function.
    -- * `f`: A function that takes a `BuiltinFunction` expression and returns a `SimplifiedExpression` representing the actual built-in
    --   function application logic. This function  performs any necessary manipulations on the arguments
    --   before calling the built-in function.
    -- * `b`: The identifier (`BuiltinFunction`) for the built-in function.
    --
    -- The generated `LambdaExpression` handles two cases:
    --
    -- 1. The argument (`$0`) is not applied:
    --     - It unpacks the value from the `boxType` using `Unbox`.
    --     - It applies the provided `f` function to the unpacked value and the built-in function identifier (`b`).
    --     - The result of `f` is then packed back into the `boxType` using a nested `Case` expression.
    -- 2. The argument (`$0`) is already applied:
    --     - It assumes the argument is applied with a single argument (`$1`).
    --     - It unpacks both the outer value from the `boxType` and the inner argument (`$1`) from the `boxType`.
    --     - It applies the provided `f` function to the built-in function identifier (`b`), the unpacked outer value
    --       (`#0`), and the unpacked inner argument (`#1`).
    --     - The result of `f` is then packed back into the `boxType` using a nested `Case` expression.
    --
    -- This function is typically used as a helper for defining built-in functions that operate on boxed values
    -- (like integers or strings) and require unpacking and repacking during function application.

    where
    rawBuiltin :: BoxedValueType -> (SimplifiedExpression -> SimplifiedExpression) -> BuiltinFunction -> LambdaExpression
    rawBuiltin boxType f b =
        LambdaExpression
        []
        N
        ["$0", "$1"]
        ( Case
            (Apply "$0" [])
            ["$1"]
            ( Unbox
                boxType
                "#0"
                ( Case
                    (Apply "$1" [])
                    ["#0"]
                    (Unbox boxType "#1" (f (BuiltinFunction b [NameAtom "#0", NameAtom "#1"])))
                )
            )
        )
    -- | Create an expression for an integer box

    -- | This function `makeIntBox` takes an expression (`e`) and returns an expression representing an integer box.
    --
    -- It uses a `Case` expression to handle the argument (`e`). Since the argument is not expected to have any
    -- cases itself (it's  the result of another expression), the case list is empty (`[]`).
    --
    -- Inside the case, it uses `BindPrim IntBox "#v"` to create a binding for the primitive type `IntBox`. The name
    -- atom `"#v"` is used as a placeholder for the actual integer value. The binding is then wrapped in a `Box`
    -- expression with `IntBox` as the box type and `NameAtom "#v"` again referencing the placeholder value.
    --
    -- In essence, this function constructs an STG expression that represents an integer box containing the provided
    -- expression (`e`) as its value.

    makeIntBox :: SimplifiedExpression -> SimplifiedExpression
    makeIntBox e =
      Case e [] (BindPrim IntBox "#v" (Box IntBox (NameAtom "#v")))

    -- | Create an expression for a string box

    -- | This function `makeStringBox` takes an expression (`e`) and returns an expression representing a string box.
    --
    -- It functions similarly to `makeIntBox`. It uses a `Case` expression with an empty case list (`[]`) to handle
    -- the argument (`e`). Inside the case, it creates a binding for the primitive type `StringBox` using
    -- `BindPrim StringBox "#v"`. The name atom `"#v"` is a placeholder for the actual string value. The binding
    -- is then wrapped in a `Box` expression with `StringBox` as the box type and `NameAtom "#v"` referencing the
    -- placeholder value.

    makeStringBox :: SimplifiedExpression -> SimplifiedExpression
    makeStringBox e =
      Case e [] (BindPrim StringBox "#v" (Box StringBox (NameAtom "#v")))

    -- | Helper for creating built-in function bindings for unboxed integers

    -- | This function `unboxedIntBuiltin` takes a built-in function identifier (`BuiltinFunction`) and returns a `LambdaExpression`
    -- representing the built-in function. It utilizes the `rawBuiltin` helper function to achieve this.
    --
    -- `unboxedIntBuiltin` is specifically designed for built-in functions that operate on unboxed integers. The
    -- `rawBuiltin` function is parameterized with:
    --
    -- * `IntBox` as the `boxType`, indicating that the function deals with integer values.
    -- * `makeIntBox` as the function (`f`) to be used for packing the result into an integer box. This ensures
    --   the result is boxed before returning from the built-in function.
    -- * The provided `BuiltinFunction` argument (`b`) as the built-in function identifier.
    --
    -- The resulting `LambdaExpression` from `rawBuiltin` handles unpacking arguments (if necessary) and repacking the
    -- result using `makeIntBox`.

    unboxedIntBuiltin :: BuiltinFunction -> LambdaExpression
    unboxedIntBuiltin = rawBuiltin IntBox makeIntBox

    -- | Helper for creating built-in function bindings for unboxed strings

    -- | This function `unboxedStringBuiltin` takes a built-in function identifier (`BuiltinFunction`) and returns a `LambdaExpression`
    -- representing the built-in function. It functions similarly to `unboxedIntBuiltin` but uses `StringBox` as the
    -- `boxType` and `makeStringBox` for packing the result.

    unboxedStringBuiltin :: BuiltinFunction -> LambdaExpression
    unboxedStringBuiltin = rawBuiltin StringBox makeStringBox

    -- | Helper for creating built-in function bindings for booleans

    -- | This function `boolBuiltin` takes a built-in function identifier (`BuiltinFunction`) and returns a `LambdaExpression`
    -- representing the built-in function. It utilizes `rawBuiltin` but with some adaptations for handling boolean values.
    --
    -- * `IntBox` is used as the `boxType` because booleans are internally represented as integers (0 for False, 1 for True)
    --   in STG.
    -- * A lambda function (`\<| \e -> ...\>`) is used as the `f` argument to `rawBuiltin`. This lambda function takes
    --   an expression (`e`) and returns a new `Case` expression.
    --   * The new case expression handles the unpacked value (`e`).
    --   * It uses `IntegerCases` to define two alternatives:
    --     *
    boolBuiltin :: BuiltinFunction -> LambdaExpression
    boolBuiltin =
      rawBuiltin IntBox <| \e ->
        Case
          e
          []
          ( IntegerCases
              [(0, Constructor 0 []), (1, Constructor 1 [])]
              Nothing
          )



-- | Compile an AST PolyType to STG

-- | Compiles an abstract syntax tree scheme (`AST PolyType`) to an STG program (`Either STGError STG`).
-- This function is the entry point for the STG compilation process. It takes an AST representing the source
-- program and returns either an `STG` program (representing the compiled code) or an `STGError` indicating
-- an error encountered during compilation.
--
-- The compilation process involves these steps:
--
-- 1. Gather information from the AST using `extractASTInfo`. This information includes built-in function
--    names and top-level definition names.
-- 2. Run the core STG monad computation (`runSTGM`) with:
--     - `astToSTG ast`: The main conversion function that transforms the AST to an STG program.
--     - `info`: The information gathered from the AST in step 1. This information is passed to the
--       monad computations within `astToSTG` and potentially other functions involved in the conversion.
compileToSTG :: AST PolyType -> Either STGError STG
compileToSTG abstractSyntaxTree =
  let info = extractASTInfo abstractSyntaxTree
  in runSTGM (astToSTG abstractSyntaxTree) info

