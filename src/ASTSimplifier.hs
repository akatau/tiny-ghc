{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- Module ASTSimplifier
-- This module defines functions for simplifying a parser abstract syntax tree (AST)
-- representing a functional language.

module ASTSimplifier (

  -- Data types for representing patterns and literals
  Match (..),
  Value (..),

  -- Data type for representing constructor definitions
  DataVariant (..),

  -- Data type representing errors during simplification
  AnalyzeError (..),

  -- Data type representing the simplified abstract syntax tree
  AST (..),

  -- Data type representing expressions in the language
  SimplifiedExpression (..),

  -- Data type representing value definitions with names, types, and expressions
  ValueDefinition (..),

  -- Type aliases for common names used in the AST
  Identifier (..),
  ValueIdentifier,
  TypeArgument,
  ConstructorName,
  TypeName,

  -- BuiltinFunction types and functions
  BuiltinFunction (..),

  -- Class for types that have access to the constructor map
  HasVariantInfo (..),
  
  -- Data type representing information about a constructor
  ConstructorInfo (..),

  -- Type alias for a map of constructor names to constructor information
  ConstructorMap (..),

  -- Function to check if an expression represents a constructor application
  hasVariant,

  -- Function to safely lookup a constructor by name, raising an error if not found
  getVariantInfo,

  -- Main entry point for simplification, taking a parser AST and returning a simplified AST or an error
  simplifier,

) where


-- Imports for the ASTSimplifier Module

import Control.Monad   -- Basic monad functions
  (
  forM_,             -- Sequence monadic computations
  replicateM,         -- Replicate a value within a monadic computation
  unless,            -- Conditional execution in monadic context
  when,               -- Conditional execution in monadic context
  )

import Control.Monad.Except  -- Monad with exception handling
  ( 
    Except,        -- Data type for representing exceptions
    runExcept,           -- Run a computation in the `Except` monad and handle exceptions
    MonadError (throwError), -- Ability to throw errors within a monad
    liftEither,         -- Lift an `Either` value into the `Except` monad
  )

import Control.Monad.Reader -- Monad with a reader environment
  (  
    MonadReader (..), -- Access values from the reader environment
    ReaderT (runReaderT),    -- Wrap a monad transformer around the `ReaderT` monad
    local,                  -- Modify the reader environment temporarily
    ask, asks, local  
  )

import Control.Monad.State -- Monad with internal state
  (  
    MonadState, -- Access and modify internal state
    StateT (runStateT),                   -- Wrap a monad transformer around the `StateT` monad
    execStateT, 
    get, 
    gets, 
    modify', 
    put                          -- Run a computation in the `StateT` monad
  )

import Data.Foldable (asum)  -- Sum all elements in a foldable structure

import Data.Function (on)    -- Apply a function point-wise to elements of two lists

import Data.List -- List manipulation functions 
  (           
    elemIndex,              -- Find the index of an element in a list
    foldl',                  -- Left fold over a list
    groupBy,                 -- Group elements of a list based on a predicate
    transpose,                -- Transpose a list of lists
  )

import qualified Data.Map as Map  -- Functions and qualified imports for maps

import Data.Maybe (catMaybes)  -- Concatenate all the `Just` values from a list of `Maybe` values

import qualified Data.Set as Set  -- Functions and qualified imports for sets

-- Custom module imports
import Ourlude  -- Import functions and types from the `Ourlude` module

-- Parser module imports
import Parser -- Data types and functions from the `Parser` module
  (  
    DataVariant (..),  -- Data type for constructor definitions
    ConstructorName,             -- Type alias for constructor names
    Value (..),                -- Data type for literals
    Identifier,                        -- Type alias for names
    ValueIdentifier,                      -- Type alias for variable names
  )

import qualified Parser as P  -- Import all functions and types from the `Parser` module with the alias `P`

-- Types module imports
import Types   -- Data types and functions from the `Types` module
  ( 
    FreeTypeVariables (..),         -- Data type for tracking free type variables
    PolyType (..),                -- Data type for representing type schemes
    Type (..),                   -- Data type for representing different kinds of types
    TypeName,                   -- Type alias for type names
    TypeArgument,                    -- Type alias for type variables
    makePolymorphic,                   -- Function to close a type scheme with respect to a type environment
  )


-- | The types of error that can happen while simplifying the AST
data AnalyzeError
  = -- | Multiple type annotations are present for the same value definition.
    MultipleTypeAnnotations ValueIdentifier [PolyType] -- -> "Multiple type annotations for the same value: {valueName} ({types})"
  | -- | Different pattern lengths have been observed for the same function definition.
    DifferentPatternLengths ValueIdentifier [Int] -- -> "Function '{valueName}' has different pattern lengths: ({patternLengths})"
  | -- | An annotation refers to a value definition that hasn't been implemented yet.
    UnimplementedAnnotation ValueIdentifier  -- -> "Unimplemented annotation for value: {valueName}"
  | -- | A type variable is used in a constructor but is not bound by the constructor's type parameters.
    UnboundTypeVarsInConstructor [TypeArgument] ConstructorName  -- -> "Unbound type variables in constructor '{constructor}': {typeVars}"
  | -- | A type name is used but cannot be resolved to a concrete type.
    UnknownType TypeName  -- -> "Unknown type: {typeName}"
  | -- | The number of arguments provided to a type constructor doesn't match its expected arity.
    MismatchedTypeArgs TypeName Int Int -- -> "Mismatched type arguments for '{typeName}': expected {expectedArgs}, got {actualArgs}"
  | -- | A type synonym refers to itself or another type in the definition chain, leading to an infinite loop.
    CyclicalTypeSynonym TypeName [TypeName] -- -> "Cyclical type synonym: {typeName} depends on itself ({chain})"
  | -- | A constructor application refers to a constructor that hasn't been defined.
    UnknownConstructor ConstructorName -- -> "Unknown constructor: {constructorName}"
  deriving (Show, Eq)


-- | The simplified abstract syntax tree (AST) representing a program after simplification.
--
-- The `AST` data type stores information about constructors and value definitions
-- after the simplification process. It is parameterized by a type `t` which can be used
-- to hold additional context or information specific to the simplification process.
-- For example, it could be used to track unique names or other bookkeeping data.
data AST t = AST {
  -- | A map from constructor names to information about those constructors.
  -- This map is used during simplification to efficiently lookup details about constructors.
  astConstructors :: ConstructorMap,
  -- | A list of value definitions present in the program.
  -- Each value definition includes its name, type annotation (optional), and expression.
  astValueDefs :: [ValueDefinition t]
  } deriving (Show, Eq)

-- | The information we have about a given constructor.
--
-- This data type stores details about a constructor used during simplification.
data ConstructorInfo = ConstructorInfo {
  -- | The arity of the constructor, which is the number of arguments it takes.
  -- This information can be derived from the constructor's type, but having it readily
  -- available improves efficiency during simplification.
  constructorArity :: Int,
  -- | The type of the constructor, represented as a type scheme.
  -- A type scheme allows for generic reasoning about types without requiring full type information.
  constructorType :: PolyType,
  -- | The tag or number associated with the constructor.
  -- This value can be used for efficient pattern matching during simplification.
  constructorNumber :: Int
  } deriving (Show, Eq)

-- | A ConstructorMap is a map from constructor names to information about them.
--
-- This type alias uses the `Map` type from the `Data.Map` module to efficiently store
-- and retrieve information about constructors based on their names.
type ConstructorMap = Map.Map ConstructorName ConstructorInfo


-- | Represents the information we might have when resolving a type name.
--
-- This data type, `TypeResolution`, is used during type resolution to track how type names are mapped to their corresponding types. There are two main cases:
--
-- * `Synonym Type`: This variant indicates that the type name is an alias for another fully resolved type expression. The `Type` constructor holds the actual resolved type.
-- * `Custom Int`: This variant represents a custom type defined within the program. The `Int` value specifies the arity (number of arguments) of the custom type.
data TypeResolution
  = Synonym Type -- -> "Type '{typeName}' is a synonym for: {resolvedType}"
  | Custom Int -- -> "Type '{typeName}' is a custom type with arity: {arity}"
  deriving (Show, Eq)

-- | A resolution map provides a way to efficiently lookup information about type names.
--
-- The `TypeMap` type alias uses the `Map` type from the `Data.Map` module to store mappings between type names (`TypeName`) and their corresponding resolving information (`TypeResolution`). This map is essential during type resolution to ensure all type names are mapped to their actual types before simplification begins.
type TypeMap = Map.Map TypeName TypeResolution

-- | A class for monadic contexts with access to type information.
--
-- The `HasVariantInfo` class represents a monadic context that provides access to the constructor map. This map is crucial during simplification for efficiently looking up information about constructors based on their names. The class requires the underlying monad to be an instance of the `Monad` type class.
class Monad m => HasVariantInfo m where
  -- | Access the type information available in this context.
  --
  -- This function, `constructorMap`, allows retrieval of the constructor map within the monadic context. The constructor map provides details about constructors used during simplification.
  constructorMap :: m ConstructorMap


-- | Given a resolution map (`TypeMap`) and a type (`Type`), fully resolve the type or throw an error.
--
-- This function, `resolve`, is responsible for taking a type and a resolution map as input. It iterates through the type structure, recursively resolving any type names encountered. The following actions are performed:
--
-- * **Synonym Replacement:** If a type name is a synonym (alias) for another type, it is replaced with its actual resolved type using the information stored in the resolution map.
-- * **Custom Type Arity Check:** For custom types, the function ensures that the number of arguments provided in the type usage (arity) matches the expected arity defined in the resolution map. Any mismatch results in an error.
-- * **Recursive Descent:** The function recursively calls itself on the argument and return types of function types (`t1 :-> t2`) to fully resolve them as well.
-- * **Terminal Types:** If the type is a terminal type (e.g., `Int`, `Bool`), it is returned as-is without further processing.
--
-- The function returns an `Either AnalyzeError Type`. 
-- * If the type is successfully resolved, the fully resolved type is returned as `Right Type`.
-- * If an error occurs during resolution (e.g., unknown type name, arity mismatch), an appropriate `AnalyzeError` is returned as `Left AnalyzeError`.
resolve :: TypeMap -> Type -> Either AnalyzeError Type
resolve mp = go
  where
    -- | Recursive helper function for type resolution
    go :: Type -> Either AnalyzeError Type
    go = \case
      -- Custom type: resolve arguments and check arity
      CustomType name ts -> do
        ts' <- mapM go ts -- Resolve arguments recursively
        let arity = length ts' -- Get actual arity
        case Map.lookup name mp of
          Nothing -> Left (UnknownType name) -- Unknown type error
          Just (Synonym _) | arity /= 0 -> Left (MismatchedTypeArgs name 0 arity) -- Arity mismatch for synonym
          Just (Synonym t) -> return t -- Replace synonym with resolved type
          Just (Custom expected) | arity /= expected -> Left (MismatchedTypeArgs name expected arity) -- Arity mismatch for custom type
          Just (Custom _) -> return (CustomType name ts') -- Return updated custom type
      -- Function type: resolve argument and result types
      t1 :-> t2 -> (:->) <$> go t1 <*> go t2
      -- Terminal type: return as-is
      terminal -> return terminal

-- | Check if a name corresponds to a constructor in the program.
--
-- This function, `hasVariant`, requires the monadic context (`m`) to be an instance of the `HasVariantInfo` class. This ensures access to the constructor map within the context. The function takes a name as input and returns a monadic value of type `Bool`.
--
-- * If the name exists in the constructor map, the function returns `True` within the monad.
-- * If the name is not found in the constructor map, the function returns `False` within the monad.
hasVariant :: HasVariantInfo m => Identifier -> m Bool
hasVariant name = Map.member name <$> constructorMap

-- | Lookup the information about a given constructor, raising an error if not found.
--
-- This function, `getVariantInfo`, also requires the monadic context (`m`) to be an instance of the `HasVariantInfo` class. It takes a `ConstructorName` as input and returns a monadic value of type `ConstructorInfo`.
--
-- * If the constructor name is found in the constructor map, the function returns the corresponding `ConstructorInfo` within the monad.
-- * If the constructor name is not found in the constructor map, the function raises a `AnalyzeError` of type `UnknownConstructor` with the name of the missing constructor. This error is propagated using the `error` function within the monad.
getVariantInfo :: HasVariantInfo m => ConstructorName -> m ConstructorInfo
getVariantInfo name = Map.findWithDefault err name <$> constructorMap
  where
    err = UnknownConstructor name |> show |> error

-- | Represents a single definition for a value in the program.
--
-- This data type, `ValueDefinition`, is parameterized over a type `t` which can be used for holding additional context or information specific to the simplification process. For example, it could be used to track unique names or other bookkeeping data.
--
-- A value definition includes the following components:
-- * `valueName`: The name of the value being defined (`ValueIdentifier`).
-- * `maybeType`: An optional type annotation provided by the user using a `Maybe PolyType`.
-- * `typeAnnotation`: A type annotation inferred during type checking (`t`). This can be different from the user-provided annotation.
-- * `expr`: The expression on the right-hand side of the definition (`SimplifiedExpression t`).
data ValueDefinition t = ValueDefinition ValueIdentifier (Maybe PolyType) t (SimplifiedExpression t) deriving (Show, Eq)

-- | Represents a built-in operation available in the language.
--
-- This data type, `BuiltinFunction`, enumerates the different built-in operations supported by the language. These operations can be used within expressions without requiring explicit function definitions.
data BuiltinFunction
  = Add        -- Addition
  | Sub        -- Subtraction
  | Mul        -- Multiplication
  | Div        -- Division
  | Compose   -- Function composition
  | Concat     -- String concatenation
  | Cash       -- Head of a list
  | Less       -- Less than comparison
  | LessEqual  -- Less than or equal comparison
  | Greater    -- Greater than comparison
  | GreaterEqual -- Greater than or equal comparison
  | EqualTo    -- Equality comparison
  | NotEqualTo  -- Inequality comparison
  | And        -- Logical AND
  | Or         -- Logical OR
  | Negate     -- Negation
  deriving (Show, Eq)

-- | Represents a kind of expression in the language.
--
-- This data type, `SimplifiedExpression`, defines the different types of expressions that can be used in the language. Each variant holds the specific information for that expression type.
data SimplifiedExpression t
  = 
    -- | Let expression: introduces local definitions and uses them in an expression
    LetExpression [ValueDefinition t] (SimplifiedExpression t)  -- -> "let definitions: {defs}; in: {expr}"
  -- | Case expression: pattern matching on an expression
  | CaseExpression (SimplifiedExpression t) [(Match, SimplifiedExpression t)]  -- -> "case {expr} of {patterns}"
  -- | Error expression: represents an error that occurred during evaluation
  | Error String -- -> "Error: {message}"
  -- | Value expression: represents a literal value like a number or string
  | LiteralExpression Value
  -- | BuiltinFunction application: applies a built-in operation to an expression
  | BuiltinFunction BuiltinFunction
  -- | Identifier expression: refers to a previously defined value
  | IdentifierExpr Identifier
  -- | Function application: applies an expression (function) to another expression (argument)
  | FunctionApplicationExpression (SimplifiedExpression t) (SimplifiedExpression t)
  -- | Lambda expression: defines an anonymous function with a name, argument, and body
  | LambdaExpression ValueIdentifier t (SimplifiedExpression t) -- -> "lambda {name} : {type}. {body}"
  deriving (Show, Eq)


-- | Represents a pattern that appears in a case expression.
--
-- This data type, `Match`, defines the different types of patterns allowed within a `CaseExpression` (case expression). Unlike the parser stage, patterns here are completely flat, meaning they cannot contain nested patterns. One of the key responsibilities of this module is to remove nested patterns encountered during parsing and flatten them into this simplified representation.
--
-- Note that simple name patterns (just a variable name) do not appear explicitly in this data type. During simplification, these are replaced with a wildcard pattern (`Wildcard`). The actual value used in the pattern matching is then obtained from the scrutinee (the expression being matched against) within the corresponding case branch.
data Match
  = -- | Wildcard pattern: matches any value
    Wildcard -- -> "Wildcard pattern"
  -- | Value pattern: matches a specific literal value (e.g., integer, string)
  | LiteralPattern Value -- -> "Value pattern: {value}"
  -- | Constructor pattern: matches a constructor applied to zero or more arguments
  | ConstructorPattern ConstructorName [ValueIdentifier] -- -> "Constructor pattern: {constructor}({arguments})"
  deriving (Show, Eq)


-- | Substitute all occurrences of a name with an expression within another expression.
--
-- This function, `replaceName`, performs name substitution within an expression. It takes three arguments:
--
-- * `old`: The name to be substituted (the one being replaced).
-- * `new`: The expression to substitute with (the replacement).
-- * `expr`: The expression where the substitution should occur.
--
-- The function recursively traverses the expression structure, replacing all occurrences of the `old` name with the `new` expression. It respects lexical scoping rules, meaning that bindings within nested `LetExpression` definitions that shadow the `old` name will not be replaced.
--
-- This function is helpful for various tasks during simplification, such as replacing local variables with their corresponding expressions.
replaceName :: Identifier -> SimplifiedExpression t -> SimplifiedExpression t -> SimplifiedExpression t
replaceName old new = go
  where
    -- Extract names from ValueDefinition records
    names :: [ValueDefinition t] -> [Identifier]
    names = map (\(ValueDefinition n _ _ _) -> n)

    -- Recursive helper function for substitution
    go (IdentifierExpr name) | name == old = new -- Replace name expression if it matches
    go (FunctionApplicationExpression f e) = FunctionApplicationExpression (go f) (go e) -- Substitute in function application
    go (CaseExpression scrut branches) = -- Substitute in case expression
      CaseExpression (go scrut) (map (second go) branches)
    go (LambdaExpression name t e) | name /= old = LambdaExpression name t (go e) -- Substitute within lambda, avoid shadowing
    go (LetExpression defs e)
      | old `notElem` names defs = -- Substitute within LetExpression, avoid shadowing
        let changeDef (ValueDefinition n dec t e') = ValueDefinition n dec t (go e')
         in LetExpression (map changeDef defs) (go e)
    go terminal = terminal -- No change for terminal expressions

-- | Construct a map of constructor information from program definitions.
--
-- This function, `buildVariantInfoMap`, takes a list of parser definitions (`[P.Definition]`) and attempts to create a `ConstructorMap`. It uses the `foldMapM` function from the `MonadError` type class, allowing it to propagate errors (of type `AnalyzeError`) during the construction process.
--
-- The function iterates through the definitions, handling specifically `P.DataDefinition`s (data type definitions). For each data type:
--   * It creates a `CustomType` representing the root constructor.
--   * It iterates over the data type's constructors and definitions, using the `makeMap` function to build the constructor information details.
--
-- The `makeMap` function is a monadic helper that:
--   * Extracts type variables from the data type definition.
--   * Constructs a type scheme for the constructor based on the return type and constructor arguments.
--   * Creates a `ConstructorInfo` record holding details like arity (number of arguments), type scheme, and constructor number.
--   * Checks for unbound type variables in the constructor's type scheme and throws a `AnalyzeError` (specifically `UnboundTypeVarsInConstructor`) if any are found. Unbound type variables indicate missing type information.
--   * Finally, it creates a map entry with the constructor name as the key and the `ConstructorInfo` record as the value.
--
-- The overall `buildVariantInfoMap` function combines the results from processing all data type definitions into a single `ConstructorMap`.
buildVariantInfoMap :: [P.Definition] -> Either AnalyzeError ConstructorMap
buildVariantInfoMap =
  foldMapM <| \case
    P.DataDefinition name typeVars definitions ->
      let root = CustomType name (map TVar typeVars)
       in foldMapM (makeMap typeVars root) (zip definitions [0 ..])
    _ -> return Map.empty
  where
    makeMap :: MonadError AnalyzeError m => [TypeArgument] -> Type -> (DataVariant, Int) -> m ConstructorMap
    makeMap typeVars ret (P.DataVariant cstr types, number) = do
      let arity = length types
          scheme = PolyType typeVars (foldr (:->) ret types)
          info = ConstructorInfo arity scheme number
          freeInScheme = freeTypeVars scheme
      unless (null freeInScheme)
        <| throwError (UnboundTypeVarsInConstructor (Set.toList freeInScheme) cstr)
      return (Map.singleton cstr info)

-- | Resolve the type within the scheme of constructor information.
--
-- This function, `resolveVariantTypes`, takes a resolution map (`TypeMap`) and a constructor map (`ConstructorMap`) as input. It performs type resolution on the type scheme (`PolyType`) stored within the constructor information for each constructor.
--
-- The function uses the `traverse` function from the `Applicative` type class to iterate over each constructor information entry in the constructor map. For each entry:
--   * It extracts the arity (number of arguments), type scheme, and constructor number from the `ConstructorInfo` record.
--   * It calls the `resolve` function to resolve the type (`t`) within the scheme using the provided resolution map (`mp`).
--   * If resolution is successful, it creates a new `ConstructorInfo` record with the same arity, constructor number, and the resolved type replacing the original type in the scheme.
--   * Any errors encountered during resolution (e.g., unknown type name, arity mismatch) are propagated using the `traverse` function.
--
-- The final result of `resolveVariantTypes` is a new constructor map where all type schemes within the constructor information have been resolved using the provided resolution map. This ensures that all types used within constructors are fully resolved before simplification begins.
resolveVariantTypes :: TypeMap -> ConstructorMap -> Either AnalyzeError ConstructorMap
resolveVariantTypes mp =
  traverse
    <| \(ConstructorInfo arity (PolyType vars t) number) -> do
      resolved <- resolve mp t
      return (ConstructorInfo arity (PolyType vars resolved) number)

{- Resolving all of the type synonyms -}

-- | Compute the set of type names that a type depends on.
--
-- This function, `findTypeDependencies`, takes a type (`Type`) as input and returns a set of `TypeName`s representing all the type names that the input type depends on. It recursively analyzes the type structure to identify all referenced type names.
--
-- The function handles different type constructors:
--   * Function types (`t1 :-> t2`): It computes the dependencies of both the argument type (`t1`) and the return type (`t2`) and combines the results using set union (`<>`).
--   * Custom types (`CustomType name exprs`):
      -- * It adds the custom type name itself (`name`) to the dependency set using `Set.singleton`.
      -- * It recursively computes the dependencies of each argument expression (`exprs`) within the custom type definition and combines them using `foldMap findTypeDependencies`.
--   * Terminal types (`Int`, `Bool`, etc.): It returns an empty set (`mempty`) as they don't depend on any other types.
findTypeDependencies :: Type -> Set.Set TypeName
findTypeDependencies = \case
  t1 :-> t2 -> findTypeDependencies t1 <> findTypeDependencies t2
  CustomType name exprs -> Set.singleton name <> foldMap findTypeDependencies exprs
  _ -> mempty

-- | State information used during type dependency sorting.
--
-- This data type, `TypeSortState`, represents the state information maintained while sorting the graph of types based on their dependencies. Sorting ensures that types are resolved in a valid order, avoiding circular dependencies between type synonyms.
--
-- The `TypeSortState` record holds two components:
--   * `unseen`: A set of `TypeName`s that haven't been processed yet (types to be sorted).
--   * `output`: A list of `TypeName`s representing the sorted order of type names discovered so far.
data TypeSortState = TypeSortState
  { unseen :: Set.Set TypeName, -- Unseen (unsorted) type names
    output :: [TypeName]    -- Output (sorted) list of type names
  }

  {- Resolving all of the type synonyms -}

-- | Monad transformer for type dependency sorting.
--
-- This type, `SorterM a`, represents a monad transformer specifically designed for performing topological sorting of the type graph based on type dependencies. It combines several monad transformers to achieve this functionality:
--
-- * `ReaderT (Set.Set TypeName)`: Provides access to a read-only set of type names (`Set.Set TypeName`). This set represents the set of ancestors encountered during the sorting process, used for cycle detection.
-- * `StateT TypeSortState (Except AnalyzeError)`: Manages the state information (`TypeSortState`) during the sorting process. This state includes the set of unseen type names and the current sorted output. It also allows for throwing exceptions (`AnalyzeError`) in case of errors.
-- * `a`: The underlying monad type (`a`) that the transformer wraps. This allows the computation to return results of type `a` at the end of the sorting process.
type SorterM a = ReaderT (Set.Set TypeName) (StateT TypeSortState (Except AnalyzeError)) a

-- | Run the type dependency sorter, given an initial state.
--
-- This function, `runSorter`, takes a `SorterM a` computation (the sorting logic) and a `TypeSortState` (initial state) as input. It uses monad transformer runner functions to execute the computation within the specified monad transformers:
--
-- * `runReaderT m Set.empty`: Runs the `SorterM` computation with an empty set of ancestors (no cycles encountered yet).
-- * `runStateT st`: Executes the computation within the provided `TypeSortState` (initial state).
-- * `runExcept`: Captures any exceptions (`AnalyzeError`) thrown during the sorting process.
-- * `fmap fst`: Extracts the result from the `Either AnalyzeError a` monad, discarding any potential error information.
--
-- The final result of `runSorter` is either a successful computation result of type `a` or a left-wrapped `AnalyzeError` indicating an error during sorting.
runSorter :: SorterM a -> TypeSortState -> Either AnalyzeError a
runSorter m st =
  runReaderT m Set.empty |> (`runStateT` st) |> runExcept |> fmap fst

-- | Sort type synonyms topologically based on their dependencies.
--
-- This function, `sortTypeSynonyms`, takes a mapping from type names (`TypeName`) to shallow types (`Type`) as input. It performs the following actions:
--
-- 1. Run the `sort` computation using `runSorter` with an initial `TypeSortState` containing:
--     * `unseen`: Set of all type names from the input map (unsorted).
--     * `output`: Empty list (no sorted types yet).
-- 2. Reverse the final sorted list obtained from the `sort` computation.
--
-- The `sortTypeSynonyms` function utilizes several helper functions for the sorting process:
--
-- * `deps`: Computes the set of type dependencies for a given type name using the provided type map (`mp`).
-- * `see`: Checks if a type name exists in the set of unseen types (`unseen`) and updates the state accordingly.
-- * `out`: Adds a type name to the sorted output list (`output`) within the state.
-- * `withAncestor`: Creates a new context with a specific type name added to the set of ancestors (used for cycle detection).
-- * `sort`: The core sorting logic, performing a depth-first search (DFS) on the type dependency graph.
-- * `dfs`: The DFS implementation, checking for cycles, handling unseen and dependent types, and recursively sorting them.
--
-- By performing topological sorting, `sortTypeSynonyms` ensures that type dependencies are resolved in a valid order, avoiding issues with circular references between type synonyms. The resulting list represents the order in which type synonyms should be processed for successful type resolution.
sortTypeSynonyms :: Map.Map TypeName Type -> Either AnalyzeError [TypeName]
sortTypeSynonyms mp = runSorter sort (TypeSortState (Map.keysSet mp) []) |> fmap reverse
  where
    -- | Compute the set of type dependencies for a type name.
    deps :: TypeName -> Set.Set TypeName
    deps k = Map.findWithDefault Set.empty k (Map.map findTypeDependencies mp)

    -- | Check if a type name is unseen and update the state.
    see :: TypeName -> SorterM Bool
    see name = do
      unseen' <- gets unseen
      modify' (\s -> s {unseen = Set.delete name unseen'})
      return (Set.member name unseen')

    -- | Add a type name to the sorted output list within the state.
    out :: TypeName -> SorterM ()
    out name = modify' (\s -> s {output = name : output s})

    -- | Create a new context with a specific ancestor type.
    withAncestor :: TypeName -> SorterM a -> SorterM a
    withAncestor = local <<< Set.insert

    -- | The core sorting logic using depth-first search (DFS).
    sort :: SorterM [TypeName]
    sort = do
      unseen' <- gets unseen
      case Set.lookupMin unseen' of
        Nothing -> gets output
        Just n -> do
          dfs n
          sort

    -- | Perform DFS on the type dependency graph.
    dfs :: TypeName -> SorterM ()
    dfs name = do
      ancestors <- ask
      when
        (Set.member name ancestors)
        (throwError (CyclicalTypeSynonym name (Set.toList ancestors)))
      new <- see name
      when new <| do
        withAncestor name (forM_ (deps name) dfs)
        out name


-- | Gather information about custom types and their arities.
--
-- This function, `gatherCustomTypes`, takes a list of parser definitions (`[P.Definition]`) as input and constructs a map (`Map.Map TypeName Int`) that associates custom type names with their arity (number of arguments). It uses `foldMap` to iterate through the definitions and process relevant cases.
--
-- The function handles specifically `P.DataDefinition`s (data type definitions):
--   * It extracts the data type name (`name`).
--   * It retrieves the list of type variables (`vars`) used in the data type definition.
--   * It creates a map entry with the data type name (`name`) as the key and the length of the type variable list (`length vars`) as the value, representing the arity of the custom type.
--   * Other definition types are ignored (`_ -> Map.empty`).
--
-- The resulting map provides a quick lookup for the arity of any custom type encountered during the simplification process.
gatherCustomTypes :: [P.Definition] -> Map.Map TypeName Int
gatherCustomTypes =
  foldMap <| \case
    P.DataDefinition name vars _ -> Map.singleton name (length vars)
    _ -> Map.empty

-- | Gather top-level type synonym definitions.
--
-- This function, `gatherTypeSynonyms`, takes a list of parser definitions (`[P.Definition]`) as input and constructs a map (`Map.Map TypeName Type`) that maps type synonym names to their corresponding shallow type expressions. It uses `foldMap` to iterate through the definitions and process relevant cases.
--
-- The function focuses on `P.TypeSynonym` definitions:
--   * It extracts the type synonym name (`name`).
--   * It retrieves the type expression (`expr`) associated with the synonym definition.
--   * It creates a map entry with the type synonym name (`name`) as the key and the type expression (`expr`) as the value.
--   * Other definition types are ignored (`_ -> Map.empty`).
--
-- This function provides a basic collection of type synonym definitions for initial processing. However, it's important to note that these are only shallow definitions and may not capture the full picture if there are nested type synonyms involved. Further processing and resolution steps might be required to handle those cases.
gatherTypeSynonyms :: [P.Definition] -> Map.Map TypeName Type
gatherTypeSynonyms =
  foldMap <| \case
    P.TypeSynonym name expr -> Map.singleton name expr
    _ -> Map.empty

-- | Monad transformer for gathering type resolution information.
--
-- This type, `MakeResolutionM a`, represents a monad transformer specifically designed for building a resolution map during type simplification. It combines several monad transformers to achieve this functionality:
--
-- * `ReaderT (Map.Map TypeName Type)`: Provides read-only access to a map of type synonyms (`typeSynMap`) containing initial type definitions. This map is used for looking up type synonyms during resolution.
-- * `StateT TypeMap (Except AnalyzeError)`: Manages the state information (`TypeMap`) being built. This state represents the final mapping between type names and their resolved types. It also allows for throwing exceptions (`AnalyzeError`) in case of errors encountered during resolution.
-- * `a`: The underlying monad type (`a`) that the transformer wraps. This allows the computation to return results of type `a` at the end of the resolution process.
type MakeResolutionM a = ReaderT (Map.Map TypeName Type) (StateT TypeMap (Except AnalyzeError)) a

-- | Gather resolution information from program definitions.
--
-- This function, `gatherResolutions`, takes a list of parser definitions (`[P.Definition]`) as input and attempts to construct a `TypeMap`. It performs the following steps:
--
-- 1. Gathers preliminary information:
--     * `customInfo`: Extracts a map of custom type names and their arities using `gatherCustomTypes`.
--     * `typeSynMap`: Gathers a map of type synonym names and their initial type expressions using `gatherTypeSynonyms`.
-- 2. Sorts type synonyms topologically using `sortTypeSynonyms`. This ensures that type dependencies are resolved in a valid order, avoiding issues with circular references.
-- 3. Runs the `resolveAll` computation using `runResolutionM`. This computation performs the actual resolution process. 
--   * `runResolutionM`: Executes the `MakeResolutionM` computation within the provided monad transformers.
-- 4. The final result is either a successful `TypeMap` containing resolved types or a left-wrapped `AnalyzeError` indicating an error during resolution.
gatherResolutions :: [P.Definition] -> Either AnalyzeError TypeMap
gatherResolutions defs = do
  let customInfo = gatherCustomTypes defs
      typeSynMap = gatherTypeSynonyms defs
  names <- sortTypeSynonyms typeSynMap
  runResolutionM (resolveAll names) typeSynMap (Map.map Custom customInfo)
  where
    -- | Run a `MakeResolutionM` computation within the monad transformers.
    --
    -- This function, `runResolutionM`, takes a `MakeResolutionM a` computation (`m`), a map of initial type synonyms (`typeSynMap`), and an initial resolution map (`st`) as input. It uses monad transformer runner functions to execute the computation within the specified monad transformers:
    --
    -- * `runReaderT m typeSynMap`: Runs the `MakeResolutionM` computation with the provided `typeSynMap` accessible for lookups.
    -- * `execStateT st`: Executes the computation within the provided initial resolution map state (`st`).
    -- * `runExcept`: Captures any exceptions (`AnalyzeError`) thrown during the resolution process.
    --
    -- The final result of `runResolutionM` is either a successful computation result of type `a` or a left-wrapped `AnalyzeError` indicating an error during resolution.
    runResolutionM :: MakeResolutionM a -> Map.Map TypeName Type -> TypeMap -> Either AnalyzeError TypeMap
    runResolutionM m typeSynMap st =
      runReaderT m typeSynMap |> (`execStateT` st) |> runExcept

    -- | Resolve all type names in a sorted list.
    --
    -- This function, `resolveAll`, takes a list of sorted type names (`[TypeName]`) as input. It iterates through the list and performs the following actions for each type name (`n`):
    --
    -- * Looks up the type definition using `asks (Map.lookup n)`. This retrieves the initial type expression associated with the name from the `typeSynMap` provided to `runResolutionM`.
    -- * If the type name is not found (`Nothing`), it throws an `UnknownType` error, indicating that the type is not defined anywhere.
    -- * If the type name is found (`Just unresolved`):
        -- * It retrieves the current resolution map state using `get`.
        -- * It attempts to resolve the type expression (`unresolved`) using `liftEither (resolve resolutions' unresolved)`. The `resolve` function performs the actual type resolution logic based on the current resolution map state (`resolutions'`).
        -- * It updates the resolution map state using `modify'`. The type name (`n`) is mapped to a `Synonym` constructor  that wraps the resolved type.
    --
    -- By iterating through the sorted list, `resolveAll` ensures that type dependencies are resolved in a valid order, avoiding issues with circular references.
    resolveAll :: [TypeName] -> MakeResolutionM ()
    resolveAll =
      mapM_ <| \n ->
          asks (Map.lookup n) >>= \case
          Nothing -> throwError (UnknownType n)
          Just unresolved -> do
              resolutions' <- get
              resolved <- liftEither (resolve resolutions' unresolved)
              modify' (Map.insert n (Synonym resolved))

-- | Context information used during type simplification.
--
-- This data type, `SimplifierContext`, represents the context information accessible within the type simplification process. It holds two key components:
--
-- * `resolutionMap`: A `TypeMap` containing mappings between type names and their resolved types. This map is built during the initial resolution phase and is used to resolve type references encountered during simplification.
-- * `constructorInfo`: A `ConstructorMap` containing information about constructors used within the program. This map stores details like arity (number of arguments) and potentially type information for each constructor.
--
-- By providing this context, the simplification process has access to essential information for resolving type references and handling constructor applications correctly.
data SimplifierContext = SimplifierContext
  { resolutionMap :: TypeMap,
    constructorInfo :: ConstructorMap
  }

-- | Type alias for the simplifier monad transformer stack.
--
-- This type alias, `ASTSimplifier a`, defines a monad transformer stack specifically designed for type simplification. It combines several monad transformers to achieve this functionality:
--
-- * `ReaderT SimplifierContext`: Provides read-only access to the current `SimplifierContext` information. This context holds the resolution map and constructor information for the simplification process.
-- * `StateT Int (Except AnalyzeError)`: Manages the state information (`Int`) used for generating generateFreshName variable names during simplification. It also allows for throwing exceptions (`AnalyzeError`) in case of errors encountered during simplification.
-- * `a`: The underlying monad type (`a`) that the transformer wraps. This allows the simplification computation to return results of type `a` at the end of the process.
--
-- By using a monad transformer stack, the simplifier can access context information, manage its internal state, and handle errors gracefully.
newtype ASTSimplifier a = ASTSimplifier (ReaderT SimplifierContext (StateT Int (Except AnalyzeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader SimplifierContext, MonadState Int, MonadError AnalyzeError)

-- | Run the simplifier computation, returning an error or a value.
--
-- This function, `runSimplifier`, takes a `SimplifierContext` (`ctx`) and a `ASTSimplifier a` computation (`m`) as input. It uses monad transformer runner functions to execute the computation within the specified monad transformers:
--
-- * `runReaderT m ctx`: Runs the `ASTSimplifier` computation with the provided context (`ctx`) accessible for reading.
-- * `runStateT 0`: Initializes the state for generateFreshName variable generation to 0.
-- * `runExcept`: Captures any exceptions (`AnalyzeError`) thrown during the simplification process.
-- * `fmap fst`: Extracts the result from the `Either AnalyzeError a` monad, discarding any potential error information.
--
-- The final result of `runSimplifier` is either a successful computation result of type `a` or a left-wrapped `AnalyzeError` indicating an error during simplification.
runSimplifier :: SimplifierContext -> ASTSimplifier a -> Either AnalyzeError a
runSimplifier ctx (ASTSimplifier m) = runReaderT m ctx |> (`runStateT` 0) |> runExcept |> fmap fst

-- | A monad transformer stack (ASTSimplifier a) that satisfies the HasVariantInfo type class constraint
instance HasVariantInfo ASTSimplifier where
  constructorMap = asks constructorInfo

-- | Generate a generateFreshName variable name within the simplifier context.
--
-- This function, `generateFreshName`, is used to generate generateFreshName variable names during the simplification process. It utilizes the `StateT` monad transformer to manage the state for generateFreshName variable generation.
--
-- The steps involved:
--   1. Retrieve the current state (`c`), which represents the counter for generateFreshName variables.
--   2. Update the state by incrementing the counter (`put (c + 1)`).
--   3. Construct the generateFreshName variable name by prepending a dollar sign (`$`) to the string representation of the counter (`show c`).
--   4. Return the generated generateFreshName variable name.
generateFreshName :: ASTSimplifier ValueIdentifier
generateFreshName = do
  c <- get
  put (c + 1)
  return ("$" ++ show c)

-- | Create a closed scheme from a type expression.
--
-- This function, `makeScheme`, takes a type expression (`typ`) as input and attempts to construct a closed scheme. It performs the following actions:
--
-- 1. Retrieves the current resolution map (`mp`) using `asks`. This map is used to resolve type references encountered in the type expression.
-- 2. Attempts to resolve the type expression (`typ`) using `liftEither (resolve mp typ)`. The `resolve` function performs the actual type resolution logic based on the current resolution map state (`mp`).
-- 3. If the resolution is successful, it applies `makePolymorphic` to the resolved type to create a closed scheme.
-- 4. The final result is either a successful closed scheme or a left-wrapped `AnalyzeError` indicating an error during resolution.
makeScheme :: Type -> ASTSimplifier PolyType
makeScheme typ = do
  mp <- asks resolutionMap
  resolved <- liftEither (resolve mp typ)
  return (makePolymorphic resolved)

-- | Validate a pattern for correctness.
--
-- This function, `validatePattern`, takes a parser pattern (`P.Match`) as input and performs basic validation checks. It handles specifically `P.ConstructorPattern` (pattern matching on a constructor):
--
-- 1. Checks if the constructor name exists using `hasVariant name`.
--   * If the constructor is not found (`ok <- hasVariant name`), it throws an `UnknownConstructor` error.
-- 2. For each sub-pattern (`pats`) within the constructor pattern, it recursively validates them using `forM_ pats validatePattern`.
--
-- This function performs only basic validation and might need further extension for more complex pattern matching scenarios.
validatePattern :: P.Match -> ASTSimplifier ()
validatePattern = \case
  P.ConstructorPattern name pats -> do
    ok <- hasVariant name
    unless ok (throwError (UnknownConstructor name))
    forM_ pats validatePattern
  _ -> return ()

{- Simplifying the Abstract Syntax Tree (AST) and Expression Tree -}

-- | Convert a parser expression to a simplified expression
--
-- This function, `expressionToSTG`, takes a parser expression (`P.SimplifiedExpression`) as input and attempts to convert it into a simplified expression within the `ASTSimplifier` monad. It performs various transformations on different expression types:
--
-- * Binary Expressions (`P.BinaryExpression`):
--   * It maps the parser operator (`op`) to a corresponding built-in function using a case expression. Supported operators include arithmetic operators (+, -, *, /), comparison operators (<, <=, >, >=, ==, !=), logical operators (and, or), and potentially others.
--   * It recursively converts the left (`e1`) and right (`e2`) subexpressions using `expressionToSTG`.
--   * It constructs an `FunctionApplicationExpression` that applies the built-in function (`BuiltinFunction b`) to the converted left (`e1'`) and right (`e2'`) subexpressions. This effectively replaces the binary expression with a function call.
-- * Negation (`P.NegateExpression`):
--   * It treats negation as a built-in function (`Negate`).
--   * It recursively converts the operand expression (`e`) using `expressionToSTG`.
--   * It constructs an `FunctionApplicationExpression` that applies the `BuiltinFunction Negate` function to the converted operand (`e'`).
-- * Where Expressions (`P.WhereExpression`):
--   * Currently, it simply converts the `WhereExpression` to a `P.LetExpression` with the same definitions and expression. This might require further handling in more complex scenarios.
-- * Conditional Expressions (`P.IfThenElseExpression`):
--   * It recursively converts the condition (`cond`), then branch (`thenn`), and else branch (`else`) expressions using `expressionToSTG`.
--   * It constructs a `CaseExpression` with the converted condition (`cond'`) as the scrutinee.
--   * It creates two literal patterns: one for `True` and another for `False`.
--   * It associates the converted then branch (`thenn'`) with the `True` pattern and the converted else branch (`elsse'`) with the `False` pattern.
-- * Variable References (`P.IdentifierExpr`):
--   * It simply returns an `IdentifierExpr` with the same name, keeping the variable reference intact.
-- * Literals (`P.LiteralExpression`):
--   * It returns a `LiteralExpression` with the same literal value, preserving the literal data.
-- * Lambda Expressions (`P.LambdaExpression`):
--   * It recursively converts the body expression (`body`) using `expressionToSTG`.
--   * It uses `foldr` with the `LambdaExpression` constructor to build the lambda expression from right to left, accumulating the converted body (`body'`) and the formal parameter names (`names`).
-- * Function Applications (`P.FunctionApplicationExpression`):
--   * It recursively converts the function expression (`f`) using `expressionToSTG`.
--   * It uses `traverse expressionToSTG exprs` to convert all argument expressions (`exprs`) within the application.
--   * It uses `foldl'` with the `FunctionApplicationExpression` constructor to apply the converted function (`f'`) to the converted arguments (`exprs'`) from left to right.
-- * Case Expressions (`P.CaseExpression`):
--   * It recursively converts the scrutinee expression (`expr`) using `expressionToSTG`.
--   * It validates each pattern within the case branches using `fst >>> validatePattern`. This performs basic checks on the pattern structure.
--   * It constructs a `Matrix` data structure by traversing the case branches. Each branch is converted to a `Row` with the pattern in the first column and the converted expression in the second column using `traverse (\(p, e) -> Row [p] <$> expressionToSTG e) patterns`.
--   * It uses `compileMatrix` to process the matrix and extract a single name (potentially a variable) and a simplified case expression. This step handles pattern matching and destructuring within case branches.
--   * Since inlining the case expression might not be desirable, it creates a `LetExpression` to bind the extracted name to the converted case expression (`caseExpression`).
-- * Let Expressions (`P.LetExpression`):
--   * It converts the value definitions (`defs`) within the let expression using `definitionsToBindings`.
--   * It recursively converts the body expression (`e`) using `expressionToSTG`.
--   * It returns a `LetExpression` with the converted definitions and body expression.
expressionToSTG :: P.SimplifiedExpression -> ASTSimplifier (SimplifiedExpression ())

-- **Binary Expressions:**
-- These are converted to applications of built-in functions based on the operator used:
-- * `P.Add` -> `Add`
-- * `P.Sub` -> `Sub`
-- * and so on for multiplication, division, composition, concatenation, etc.
expressionToSTG (P.BinaryExpression op e1 e2) = do
  let b = case op of
        P.Add -> Add
        P.Sub -> Sub
        P.Mul -> Mul
        P.Div -> Div
        P.Compose -> Compose
        P.Concat -> Concat
        P.Cash -> Cash
        P.Less -> Less
        P.LessEqual -> LessEqual
        P.Greater -> Greater
        P.GreaterEqual -> GreaterEqual
        P.EqualTo -> EqualTo
        P.NotEqualTo -> NotEqualTo
        P.And -> And
        P.Or -> Or
  e1' <- expressionToSTG e1
  e2' <- expressionToSTG e2
  return (FunctionApplicationExpression (FunctionApplicationExpression (BuiltinFunction b) e1') e2')

-- **Negation:**
-- Negation is also converted to an application of the built-in `Negate` function.
expressionToSTG (P.NegateExpression e) = FunctionApplicationExpression (BuiltinFunction Negate) <$> expressionToSTG e

-- **`where` Expressions:**
-- These are currently converted to `let` expressions. This might be revisited for potential optimization or specific handling of `where` semantics.
expressionToSTG (P.WhereExpression e defs) =
  expressionToSTG (P.LetExpression defs e)

-- **Conditional Expressions (`if`):**
-- Converted to a `CaseExpression` with a boolean condition and corresponding branches for true and false cases.
expressionToSTG (P.IfThenElseExpression cond thenn elsse) = do
  cond' <- expressionToSTG cond
  thenn' <- expressionToSTG thenn
  elsse' <- expressionToSTG elsse
  return
    ( CaseExpression
        cond'
        [ (LiteralPattern (BooleanLiteral True), thenn'),
          (LiteralPattern (BooleanLiteral False), elsse')
        ]
    )

-- **Variable References:**
-- These remain unchanged.
expressionToSTG (P.IdentifierExpr name) = return (IdentifierExpr name)

-- **Literals:**
-- These remain unchanged.
expressionToSTG (P.LiteralExpression litt) = return (LiteralExpression litt)

-- **Lambda Expressions:**
-- Converted to a nested `LambdaExpression` structure by iteratively applying the `LambdaExpression` constructor with the body and variable names.
expressionToSTG (P.LambdaExpression names body) = do
  body' <- expressionToSTG body
  return (foldr (`LambdaExpression` ()) body' names)

-- **Function Applications:**
-- The function expression and argument expressions are converted individually, and then folded together using `foldl' FunctionApplicationExpression` to create a nested application structure.
expressionToSTG (P.FunctionApplicationExpression f exprs) = do
  f' <- expressionToSTG f
  exprs' <- traverse expressionToSTG exprs
  return (foldl' FunctionApplicationExpression f' exprs')

-- **Case Expressions:**
-- These are converted to a `LetExpression` with a temporary variable holding the result of converting the case scrutinee expression (`expr`). The converted patterns are validated using `validatePattern`, and then a `Matrix` data structure is constructed to represent the case branches. 
-- Finally, the matrix is compiled into a single variable name and a nested case expression (`caseExpression`).
expressionToSTG (P.CaseExpression expr patterns) = do
  expr' <- expressionToSTG expr
  forM_ patterns (fst >>> validatePattern)
  matrix <- Matrix <$> traverse (\(p, e) -> Row [p] <$> expressionToSTG e) patterns
  -- We're guaranteed to have a single name, because we have a single column
  (names, caseExpression) <- compileMatrix matrix
  -- We create a let, because inlining isn't necessarily desired
  return (LetExpression [ValueDefinition (head names) Nothing () expr'] caseExpression)

-- **Let Expressions:**
-- The value definitions are converted first, and then the body expression is converted. The final result is a `LetExpression` with the converted definitions and body expression.
expressionToSTG (P.LetExpression defs e) = do
  defs' <- definitionsToBindings defs
  e' <- expressionToSTG e
  return (LetExpression defs' e')





-- | Convert a list of parser value definitions to a list of simplified value definitions.
--
-- This function, `definitionsToBindings`, takes a list of parser value definitions (`[P.ValueDefinition]`) as input and attempts to convert them to a list of simplified value definitions of type `[ValueDefinition ()]`. It performs the following steps:
--
-- 1. Groups value definitions by name using `groupBy ((==) `on` getName)`. This ensures that all definitions for the same variable are grouped together.
-- 2. Applies the `traverse gather` function to each group. The `gather` function performs the actual conversion for a group of value definitions with the same name.
--
-- **Helper Functions:**
--
-- * `getName`: Extracts the variable name from a value definition.
-- * `getTypeAnnotations`: Extracts any type annotations present in the value definitions within a group.
-- * `makeMatrix`: Constructs a `Matrix` data structure representing the case branches for the converted value definition.
-- * `gather`: Converts a group of value definitions with the same name to a single simplified value definition.
--
definitionsToBindings :: [P.ValueDefinition] -> ASTSimplifier [ValueDefinition ()]
definitionsToBindings =
  groupBy ((==) `on` getName) >>> traverse gather
  where
    getName :: P.ValueDefinition -> Identifier
      -- Extract the variable name from the value definition.
    getName = \case
      P.TypeAnnotation name _ -> name
      P.NameDefinition name _ _ -> name

    getTypeAnnotations :: [P.ValueDefinition] -> [Type]
      -- Extract type annotations from the value definitions within a group.
    getTypeAnnotations ls = ls |> map pluckAnnotation |> catMaybes
      where
        pluckAnnotation = \case
          P.TypeAnnotation _ typ -> Just typ
          _ -> Nothing

    makeMatrix :: [P.ValueDefinition] -> ASTSimplifier (Matrix (SimplifiedExpression ()))
      -- Construct a matrix representing case branches based on the value definition group.
    makeMatrix = traverse makeRow >>> fmap (catMaybes >>> Matrix)
      where
        makeRow = \case
          P.NameDefinition _ pats body -> do
            forM_ pats validatePattern  -- Validate patterns within case branches
            body' <- expressionToSTG body   -- Convert the body expression
            return (Just (Row pats body'))
          _ -> return Nothing

    gather :: [P.ValueDefinition] -> ASTSimplifier (ValueDefinition ())
    -- Convert a group of value definitions with the same name to a single definition.
    gather [] = error "groupBy returned empty list"  -- Handle unexpected empty group
    gather valueHeads = do
      let name = getName (head valueHeads)  -- Extract the variable name
      annotations <- getTypeAnnotations valueHeads |> mapM makeScheme -- Extract and resolve type annotations
      schemeExpr <- case annotations of
        [] -> return Nothing  -- No type annotations
        [single] -> return (Just single)  -- Single type annotation
        tooMany -> throwError (MultipleTypeAnnotations name tooMany)  -- Multiple type annotations error
      matrix <- makeMatrix valueHeads  -- Construct the case matrix
      validateMatrix name matrix  -- Validate the constructed matrix
      (names, caseExpression) <- compileMatrix matrix  -- Compile the matrix into a case expression
      let expr = foldr (`LambdaExpression` ()) caseExpression names  -- Build the final lambda expression
      return (ValueDefinition name schemeExpr () expr)  -- Return the simplified value definition





{- Match Matching Simplifying -}

-- | Check if a pattern is not a wildcard pattern.
--
-- This function, `notWildcard`, takes a parser pattern (`P.Match`) as input and determines if it's not a wildcard pattern. It performs a simple check:
--
-- * If the pattern is a `P.WildcardPattern`, it returns `False`. Wildcards are considered non-specific and might not be desirable in all cases.
-- * Otherwise, it returns `True`.
--
notWildcard :: P.Match -> Bool
notWildcard P.WildcardPattern = False
notWildcard _ = True

-- | Swap two elements in a list by their indices.
--
-- This function, `swap`, takes an index (`i`) and a list (`xs`) as input and swaps the element at index `i` with the element at index 0 (the first element). It performs the following steps:
--
-- 1. Extracts the element at index `i` using `xs !! i`.
-- 2. Filters the list of indices and corresponding elements using `zip [0 ..] xs` to create a list of pairs. Then, it filters out the pair where the first element (index) is not equal to `i`. This ensures we don't swap the element with itself.
-- 3. Extracts the remaining elements (excluding the element at index `i`) using `map snd`.
-- 4. Constructs the new list by prepending the element at index `i` (extracted in step 1), followed by the remaining elements (extracted in step 3).
--
-- This function utilizes for simplicity and might be optimized for performance-critical scenarios.
swap :: Int -> [a] -> [a]
swap i xs = (xs !! i) : (zip [0 ..] xs |> filter ((/= i) . fst) |> map snd)


-- | Represents a matrix data structure.
--
-- This newtype declaration, `Matrix a`, defines a new data type called `Matrix` that holds a list of `Row` elements of type `a`. This data structure represents a matrix of patterns encountered during pattern matching simplification. It might have multiple columns, as generated by function definitions with multiple arguments.
newtype Matrix a = Matrix [Row a] deriving (Show, Eq)

-- | Validate the coherence of a pattern matrix.
--
-- This function, `validateMatrix`, takes a monad transformer error context (`MonadError AnalyzeError m`), a variable name (`ValueIdentifier`), and a `Matrix a` as input. It performs validations on the matrix to ensure it's well-formed:
--
-- 1. Extracts the pattern lengths from each row using `map (rowPats >>> length) rows`. This gives a list of lengths for each row.
-- 2. Checks if all rows have the same length using `null lengths || all (== head lengths) lengths`. If `null lengths`, it means there are no rows (empty matrix). Otherwise, it checks if all lengths are equal to the first element (head) of the list.
--   * If the lengths are not equal, it throws a `DifferentPatternLengths` error with the name and list of lengths.
-- 3. Checks if the matrix is empty (`null rows`). If it is, it throws an `UnimplementedAnnotation` error with the name, indicating no definitions for that annotation.
-- 4. If the validations pass, the function returns successfully without throwing any errors.
--
validateMatrix :: MonadError AnalyzeError m => ValueIdentifier -> Matrix a -> m ()
validateMatrix name (Matrix rows) = do
  let lengths = map (rowPats >>> length) rows
      allEqual = null lengths || all (== head lengths) lengths
  unless allEqual
    <| throwError (DifferentPatternLengths name lengths)
  when (null rows)
    <|
    -- If we're validating a matrix for some name, and there are no patterns,
    -- then there are no definitions for that annotation
    throwError (UnimplementedAnnotation name)
  return ()

-- | Represents a row within a pattern matrix.
--
-- This data type declaration, `data Row a`, defines a record type called `Row` that holds two components:
--
-- * `rowPats`: A list of `P.Match` elements, representing the patterns in the row.
-- * `rowVal`: An element of type `a`, representing the value associated with the patterns in the row.
--
-- This `Row` data type is  used to represent a single row in the `Matrix` used for pattern matching simplification. Each row might contain multiple patterns and a corresponding value.
data Row a = Row
  { rowPats :: [P.Match],
    rowVal :: a
  }
  deriving (Show, Eq)

-- | Check if all patterns in a row are wildcards.
--
-- This function, `allWildCards`, takes a `Row a` as input and determines if all patterns within the row are `P.WildcardPattern` (wildcard patterns). It performs a simple check using `rowPats >>> all (== P.WildcardPattern)`. This applies the `all` function to the list of patterns after extracting them using `rowPats`. The `all` function returns `True` only if all elements in the list satisfy the given predicate (in this case, being equal to `P.WildcardPattern`).
--
allWildCards :: Row a -> Bool
allWildCards = rowPats >>> all (== P.WildcardPattern)


-- | Convert a list of patterns to a list of branches.
--
-- This function, `gatherBranches`, takes a list of `P.Match` as input and attempts to convert them into a list of `Branch` elements. It performs the following steps:
--
-- 1. Uses `foldMap pluckHead` to iterate over the list of patterns and apply the `pluckHead` function to each pattern. The `foldMap` function applies a function to each element of a list and combines the results using a monoid (in this case, `Set.union` for sets).
-- 2. The `pluckHead` function itself analyzes the pattern type:
--   * For `P.LiteralPattern`: Creates a single-element set containing a `LiteralBranch` with the literal value.
--   * For `P.ConstructorPattern`: Creates a single-element set containing a `ConstructorBranch` with the constructor name and number of arguments (length of sub-patterns).
--   * For other pattern types: Returns an empty set.
-- 3. Finally, the result of `foldMap pluckHead` is converted to a list using `Set.toList`.
--
-- This function performs a basic pattern transformation for handling literals and constructors during pattern matching simplification. 
--
gatherBranches :: [P.Match] -> [Branch]
gatherBranches = foldMap pluckHead >>> Set.toList
  where
    pluckHead :: P.Match -> Set.Set Branch
    pluckHead = \case
      P.LiteralPattern l ->
        Set.singleton (LiteralBranch l)
      P.ConstructorPattern name pats ->
        Set.singleton (ConstructorBranch name (length pats))
      _ -> Set.empty

-- | Extract all columns from a pattern matrix.
--
-- This function, `columns`, takes a `Matrix a` as input and returns a list of lists of `P.Match` elements. It performs the following steps:
--
-- 1. Extracts the list of rows using pattern matching on the `Matrix` constructor.
-- 2. Applies `map rowPats` to each row to extract a list of patterns from each row.
-- 3. Uses `transpose` to convert the list of row-pattern lists into a list of column-pattern lists. This effectively swaps rows and columns.
--
columns :: Matrix a -> [[P.Match]]
columns (Matrix rows) = rows |> map rowPats |> transpose

-- | Find the index of the next non-wildcard column in a matrix.
--
-- This function, `nextColumn`, takes a `Matrix a` as input and attempts to find the index of the next column that contains at least one non-wildcard pattern. It performs the following steps:
--
-- 1. Extracts all columns using `columns`.
-- 2. Applies `map (any notWildcard)` to each column list. This uses `any` to check if there's at least one pattern in the list that satisfies the `notWildcard` predicate (not a wildcard pattern).
-- 3. Uses `elemIndex True` to search for the first occurrence of `True` in the resulting list. `True` indicates a column with at least one non-wildcard pattern.
-- 4. If a valid index is found, it's wrapped in a `Maybe Int` and returned. Otherwise, `Nothing` is returned.
--
nextColumn :: Matrix a -> Maybe Int
nextColumn = columns >>> map (any notWildcard) >>> elemIndex True

-- | Swap the nth column of a matrix with the first column.
--
-- This function, `swapColumn`, takes an index (`Int`) and a `Matrix a` as input and swaps the nth column with the first column. It performs the following steps:
--
-- 1. Extracts the list of row values (`vals`) and list of row patterns (`pats`) using pattern matching on the `Matrix` constructor.
-- 2. Transposes the pattern list (`pats`) using `transpose` to convert it from rows to columns for easier manipulation.
-- 3. Applies `swap index` to the transposed pattern list to swap the nth column with the first column.
-- 4. Transposes the modified pattern list back to rows using `transpose`.
-- 5. Constructs a new `Matrix` by zipping the modified patterns (`transformed`) with the original row values (`vals`) using `zipWith Row`.
--
-- This function utilizes efficient list manipulation techniques for performance.
swapColumn :: Int -> Matrix a -> Matrix a
swapColumn index (Matrix rows) =
  let vals = map rowVal rows
      pats = map rowPats rows
      transformed = pats |> transpose |> swap index |> transpose
   in Matrix (zipWith Row transformed vals)

-- | Extract the first variable name from the first column of a matrix.
--
-- This function, `firstName`, takes a `Matrix a` as input and attempts to extract the first variable name present in the first column. It performs the following steps:
--
-- 1. Extracts the list of rows using pattern matching on the `Matrix` constructor.
-- 2. Applies `map stripName` to each row. The `stripName` function attempts to extract the name from the first pattern in the row (assuming the first column contains variable names).
-- 3. Uses `asum` to combine the results from `map stripName`. `asum` folds over a list of `Maybe` values, returning the first `Just` value or `Nothing` if all values are `Nothing`.
--
firstName :: Matrix a -> Maybe String
firstName (Matrix rows) = map stripName rows |> asum
  where
    stripName :: Row a -> Maybe String
      -- Attempt to extract the name from the first pattern (assuming variable names in the first column)
    stripName (Row (P.NamePattern n : _) _) = Just n
    stripName other = Nothing

-- | Calculate the resulting matrix after choosing the default branch.
--
-- This function, `defaultMatrix`, takes a `Matrix a` as input and calculates the resulting matrix after choosing the default branch for all rows. It performs the following steps:
--
-- 1. Extracts the list of rows using pattern matching on the `Matrix` constructor.
-- 2. Filters the rows using `isDefault`. A row is considered a default if it's empty (`[]`), contains only a wildcard pattern (`P.WildcardPattern`), or starts with a variable name (`P.NamePattern`).
-- 3. Applies `map stripHead` to the filtered rows. The `stripHead` function removes the first pattern from each row, assuming the first pattern represents the default branch.
-- 4. Constructs a new `Matrix` from the filtered and modified rows.
--
-- This function effectively simplifies the matrix by removing non-default branches (wildcards or variable names) and keeping the corresponding associated values.
defaultMatrix :: Matrix a -> Matrix a
defaultMatrix (Matrix rows) =
  rows |> filter isDefault |> map stripHead |> Matrix
  where
    isDefault :: Row a -> Bool
    -- Check if the row represents a default branch
    isDefault = \case
      Row [] _ -> True  -- Empty row
      Row (P.WildcardPattern : _) _ -> True  -- Wildcard pattern
      Row (P.NamePattern _ : _) _ -> True  -- Starts with variable name
      _ -> False

    stripHead :: Row a -> Row a
    -- Remove the first pattern (assuming default branch)
    stripHead (Row pats a) = Row (tail pats) a

-- | Calculate the resulting matrix after taking a specific branch.
--
-- This function, `branchMatrix`, takes a `Branch` (`LiteralBranch` or `ConstructorBranch`) and a `Matrix a` as input and calculates the resulting matrix after taking the specified branch. It performs the following steps:
--
-- 1. Iterates over each row in the matrix using `map`.
-- 2. Applies a function to each row that attempts to match the branch with the first pattern in the row.
--   * The `matches` function handles different branch types:
--     * `LiteralBranch`: Matches with a literal pattern if the values are equal.
--     * `ConstructorBranch`: Matches with a constructor pattern if the names and number of arguments (arity) are equal.
--   * If a match is found, it returns a `Just` value containing the remaining patterns after the matched pattern (tail). Otherwise, it returns `Nothing`.
-- 3. Uses `catMaybes` to filter out any `Nothing` values from the list of results from iterating over rows.
-- 4. Constructs a new `Matrix` from the filtered and potentially modified rows.
--
-- This function effectively filters the matrix based on the chosen branch and keeps only the remaining patterns that should be used for further matching.
branchMatrix :: Branch -> Matrix a -> Matrix a
branchMatrix branch (Matrix rows) =
  rows |> map (\(Row pats a) -> (`Row` a) <$> newPats pats) |> catMaybes |> Matrix
  where
    matches :: Branch -> P.Match -> Maybe [P.Match]
    -- Attempt to match the branch with the first pattern in a row
    matches (LiteralBranch l) (P.LiteralPattern l') | l == l' = Just []
    matches (ConstructorBranch name _) (P.ConstructorPattern name' pats)
      | name == name' =
        Just pats
    matches _ _ = Nothing

    makeWildCards :: Branch -> [P.Match]
    -- Create a list of wildcards based on the branch type (arity for constructors)
    makeWildCards (LiteralBranch _) = []
    makeWildCards (ConstructorBranch _ arity) = replicate arity P.WildcardPattern

    newPats :: [P.Match] -> Maybe [P.Match]
    -- Update the pattern list based on the match result and branch type
    newPats = \case
      P.WildcardPattern : rest ->
        Just (makeWildCards branch ++ rest)  -- Prepend wildcards for defaults
      P.NamePattern _ : rest ->
        Just (makeWildCards branch ++ rest)  -- Prepend wildcards for defaults
      pat : rest -> (++ rest) <$> matches branch pat  -- Apply matching function
      [] -> Just []  -- Empty list (no match)

-- | Represents a decision tree used for generating a case expression.
--
-- This data type declaration, `data Tree a`, defines a record type called `Tree` that holds different possibilities encountered during pattern matching simplification. It acts like a decision tree, guiding the simplification process based on the patterns and values encountered. The type parameter `a` represents the type of value associated with the tree branches.
--
-- The different constructors of `Tree` represent various steps or outcomes in the decision tree:
--
-- * `Fail`: Represents a failure in the pattern matching process, indicating no successful match was found.
-- * `Leaf a`: Represents a successful match, where a value of type `a` is the final result.
-- * `Swap Int (Tree a)`: Represents an instruction to swap the current value at a specific index (`i`) with the first value encountered in the matching process. The remaining steps are captured by the embedded `Tree a`.
-- * `SubstOut Identifier (Tree a)`: Represents an instruction to substitute all occurrences of a given variable name (`Identifier`) with the name of the first value encountered during matching. The remaining steps are captured by the embedded `Tree a`.
-- * `Select [(Branch, Tree a)] (Tree a)`: Represents the actual branching point in the decision tree. It holds a list of pairs where the first element is a `Branch` type (representing a specific pattern) and the second element is a `Tree a` (representing the remaining steps for that branch). The last element in the list acts as the default branch, used when no other branches match.
--
-- This `Tree` data type is used to represent the intermediate stages and final outcome of simplifying pattern matching expressions.
data Tree a =
  Fail
  | Leaf a
  | Swap Int (Tree a)
  | SubstOut Identifier (Tree a)
  | Select [(Branch, Tree a)] (Tree a)
  deriving (Show, Eq)

-- | Represents a type of branch in the decision tree.
--
-- This data type declaration, `data Branch`, defines two constructors that represent different types of branches encountered during pattern matching simplification:
--
-- * `ConstructorBranch ConstructorName Int`: Represents a branch for a constructor pattern. It holds the constructor name (`ConstructorName`) and the number of arguments (arity) of the constructor.
-- * `LiteralBranch Value`: Represents a branch for a literal pattern. It holds the literal value (`Value`) itself.
--
-- These branches are used within the `Select` constructor of the `Tree` data type to represent different possibilities based on the first value encountered during matching.
--
data Branch =
  ConstructorBranch ConstructorName Int
  | LiteralBranch Value
  deriving (Eq, Ord, Show)

-- | Build a decision tree from a pattern matching matrix.
--
-- This function, `buildTree`, takes a `Matrix a` as input and attempts to construct a `Tree a` representing the decision tree for generating a case expression based on the pattern matching process. It performs the following steps:
--
-- 1. Checks if the matrix is empty (`[]`). If it is, a `Fail` tree is returned, indicating no successful match.
-- 2. Checks if the first row contains only wildcards using `allWildCards`. If it does, the value associated with the first row (`rowVal r`) is extracted and wrapped in a `Leaf` tree, representing a successful match with that value.
-- 3. Otherwise, it proceeds with the following steps:
--   * Finds the index of the next non-wildcard column using `nextColumn`.
--   * If no non-wildcard column is found, it throws an error ("There must be a non wildcard in one of the rows") as a valid tree cannot be built without any non-wildcard patterns.
--   * If the next non-wildcard column is the first column (index 0):
--     * Extracts the first column patterns using `head (columns mat)`.
--     * Iterates over the patterns using `map makeTree`. The `makeTree` function creates a pair where the first element is a `Branch` constructed from the current pattern using `gatherBranches col` and the second element is a `Tree` built recursively by calling `buildTree` with the matrix resulting from taking that branch (`branchMatrix branch mat`).
--     * These pairs are collected into a list of branches (`branches`).
--     * Builds a `Tree` using `Select` constructor. This tree holds the list of branches (`branches`) and a default branch (`default'`), which is built by calling `buildTree` with the matrix resulting from choosing the default branch (`defaultMatrix mat`).
--     * Checks for the presence of a variable name in the first column using `firstName mat`.
--     * If a name is found (`Just n`), it applies a substitution (`SubstOut n`) to the base tree (`baseTree`). This substitution replaces all occurrences of the variable name with the name of the first value encountered during matching.
--     * If no name is found (`Nothing`), the base tree itself is returned.
--   * If the next non-wildcard column is not the first column (index `n`):
--     * Performs a swap operation (`Swap n`) on the matrix, moving the non-wildcard column to the first position for easier processing.
--     * Builds the decision tree recursively by calling `buildTree` with the swapped matrix.
--
-- This function is the core of the pattern matching simplification process. It analyzes the matrix, builds the decision tree based on pattern types and values, and incorporates variable name substitutions if necessary.
buildTree :: Matrix a -> Tree a
buildTree (Matrix []) = Fail
buildTree (Matrix (r : _)) | allWildCards r = Leaf (rowVal r)
buildTree mat = case nextColumn mat of
  Nothing -> error "There must be a non wildcard in one of the rows"
  Just 0 ->
    let col = head (columns mat)
        makeTree branch = (branch, buildTree (branchMatrix branch mat))
        branches = gatherBranches col |> map makeTree
        default' = buildTree (defaultMatrix mat)
        baseTree = Select branches default'
     in case firstName mat of
          Just n -> SubstOut n baseTree
          Nothing -> baseTree
  Just n -> Swap n (buildTree (swapColumn n mat))


-- | Fold a decision tree into a final case expression.
--
-- This function, `foldTree`, takes a `Tree (SimplifiedExpression t)` (decision tree with expressions as values) and an initial pattern count (`Int`) as input and folds the tree downwards, generating a final `ASTSimplifier ([ValueIdentifier], SimplifiedExpression t)` representing the simplified case expression. It performs the following steps:
--
-- 1. Generates a list of generateFreshName variable names (`names`) using `replicateM patCount generateFreshName`. The number of names corresponds to the initial pattern count (`patCount`).
-- 2. Recursively folds the tree using `go names theTree`. The `go` function takes the list of names and the tree as input and returns a `ASTSimplifier (SimplifiedExpression t)` representing the final expression.
-- 3. Returns a tuple containing the generated variable names (`names`) and the final expression (`expr`).
--
-- The `foldTree` function utilizes a helper function `go` for recursive tree folding and another helper function `handleBranch` to process individual branches within the `Select` constructor.
--
-- * `handleBranch`: Takes a list of variable names (`names`) and a branch-tree pair (`(Branch, Tree (SimplifiedExpression t))`) as input. It analyzes the branch type:
--   * `LiteralBranch`: Returns a `LiteralPattern` paired with the result of recursively folding the sub-tree (`go names tree`).
--   * `ConstructorBranch`: Generates generateFreshName names for the constructor arguments using `replicateM arity generateFreshName`. Folds the sub-tree with the combined list of names (`newNames ++ names`) and returns a `ConstructorPattern` with the constructor name and generated names paired with the folded expression.
--
-- * `go`: Takes a list of variable names (`names`) and a tree (`Tree (SimplifiedExpression t)`) as input. It performs a case analysis on the tree type:
--   * `Fail`: Returns an error expression ("Match Match Failure").
--   * `Leaf`: Returns the expression stored in the leaf node.
--   * `Swap`: Swaps elements in the name list at a specific index (`i`) and recursively folds the sub-tree with the swapped list.
--   * `SubstOut`: Applies a substitution for a variable name (`old`) with the first name in the list (`IdentifierExpr (head names)`). Then folds the sub-tree with the original name list.
--   * `Select`:
--     * Extracts the remaining names (`rest`) and the scrutinizand name (`head names`) from the name list.
--     * Uses `traverse` to iterate over the branches, applying `handleBranch` to each branch and remaining names to generate a list of pattern-expression pairs.
--     * Folds the default branch (`default'`) with the remaining names.
--     * Returns a `CaseExpression` with the scrutinizand (`scrut`), a list of branch cases (`branchCases`), and the default expression (`defaultExpr`). The branch cases are a combination of the folded branches and a wildcard pattern with the default expression.
--
foldTree :: Int -> Tree (SimplifiedExpression t) -> ASTSimplifier ([ValueIdentifier], SimplifiedExpression t)
foldTree patCount theTree = do
  names <- replicateM patCount generateFreshName
  expr <- go names theTree
  return (names, expr)
  where
    -- Handle a single branch in the decision tree
    handleBranch :: [String] -> (Branch, Tree (SimplifiedExpression t)) -> ASTSimplifier (Match, SimplifiedExpression t)
    handleBranch names (branch, tree) = case branch of
      LiteralBranch l -> (LiteralPattern l,) <$> go names tree  -- Convert to literal pattern
      ConstructorBranch cstr arity -> do  -- Handle constructor branch
        newNames <- replicateM arity generateFreshName  -- Generate generateFreshName names for arguments
        expr <- go (newNames ++ names) tree  -- Recursively fold with new names
        return (ConstructorPattern cstr newNames, expr)  -- Return constructor pattern and expression

    -- Recursive helper function for folding the tree
    go :: [ValueIdentifier] -> Tree (SimplifiedExpression t) -> ASTSimplifier (SimplifiedExpression t)
    go names tree = case tree of
      Fail -> return (Error "Match Match Failure")  -- Handle failure
      Leaf expr -> return expr  -- Base case: return the expression
      Swap i tree' -> go (swap i names) tree'  -- Swap names for swap node
      SubstOut old tree' -> replaceName old (IdentifierExpr (head names)) <$> go names tree'  -- Substitute variable name
      Select branches default' -> do  -- Handle selection node
        let rest = tail names  -- Remaining names for branches
            scrut = IdentifierExpr (head names)  -- Scrutinee (variable being matched)
        branchCases <- traverse (handleBranch rest) branches  -- Fold branches
        defaultExpr <- go rest default'  -- Fold default branch
        return <| case branchCases of  -- Choose between branches or default
          [] -> defaultExpr  -- No branches matched, use default
          _ -> CaseExpression scrut (branchCases ++ [(Wildcard, defaultExpr)])  -- Case expression with branches and default


-- | Compile a pattern matching matrix into a simplified lambda expression.
--
-- This function, `compileMatrix`, takes a `Matrix (SimplifiedExpression t)` as input and attempts to compile it into a simplified lambda expression represented by a `ASTSimplifier ([ValueIdentifier], SimplifiedExpression t)`. It performs the following steps:
--
-- 1. Takes a `Matrix (SimplifiedExpression t)` as input, which represents the pattern matching matrix to be compiled.
-- 2. Deconstructs the matrix using pattern matching and assigns it to a variable `mat`.
-- 3. Extracts the number of patterns in the first row using `length (rowPats (head rows))`. This represents the number of initial values being matched against and is stored in `patCount`.
-- 4. Calls the `buildTree` function (defined previously) to build a decision tree (`Tree (SimplifiedExpression t)`) from the pattern matching matrix (`mat`). This function analyzes the matrix patterns, builds the decision tree based on different possibilities, and incorporates variable name substitutions if necessary.
-- 5. Calls the `foldTree` function (defined previously) to fold the decision tree (`Tree (SimplifiedExpression t)`) down to a final `ASTSimplifier ([ValueIdentifier], SimplifiedExpression t)`. This function uses the generated variable names (`patCount`) and recursively folds the tree, handling different tree nodes and constructing the final case expression.
--
-- The `compileMatrix` function acts as the entry point for the compilation process. It leverages the `buildTree` and `foldTree` functions (previously documented) to convert the pattern matching matrix into a simplified lambda expression.
compileMatrix :: Matrix (SimplifiedExpression t) -> ASTSimplifier ([ValueIdentifier], SimplifiedExpression t)
compileMatrix mat@(Matrix rows) =
  let patCount = length (rowPats (head rows))
   in buildTree mat |> foldTree patCount


-- | Convert a list of pattern definitions to a list of simplified value definitions.
--
-- This function, `convertDefinitions`, takes a list of `P.Definition` (potentially containing various definition types) as input and attempts to convert them into a list of simplified `ValueDefinition ()`. It performs the following steps:
--
-- 1. Applies `map pluckValueDefinition` to the list of definitions. The `pluckValueDefinition` function (defined below) extracts only the `P.ValueDefinition` constructors from the list, discarding other definition types.
-- 2. Uses `catMaybes` to filter out any `Nothing` values resulting from `pluckValueDefinition`.
-- 3. Calls the `definitionsToBindings` function  to further simplify the extracted value definitions.
--
-- This function essentially filters and prepares the value definitions from a potentially mixed list of definitions for further processing.
convertDefinitions :: [P.Definition] -> ASTSimplifier [ValueDefinition ()]
convertDefinitions = map pluckValueDefinition >>> catMaybes >>> definitionsToBindings
  where
    -- Extract only P.ValueDefinition constructors from a definition
    pluckValueDefinition :: P.Definition -> Maybe P.ValueDefinition
    pluckValueDefinition (P.ValueDefinition v) = Just v
    pluckValueDefinition other = Nothing

-- | The main simplifier function for pattern matching expressions.
--
-- This function, `simplifier`, takes a `P.AST` (representing the abstract syntax tree of the pattern matching expression) as input and attempts to simplify it. It returns an `Either AnalyzeError (AST ())` type, indicating either successful simplification with the resulting simplified AST or an error encountered during simplification. It performs the following steps:
--
-- 1. Deconstructs the `P.AST` using pattern matching and extracts the list of definitions (`defs`).
-- 2. Calls the `gatherResolutions` function to gather resolution information from the definitions. This involves collecting information about variable names and their bindings.
-- 3. Calls the `buildVariantInfoMap` function to gather information about constructor definitions from the definitions. This involves building a map of constructor names to their argument types.
-- 4. Calls the `resolveVariantTypes` function to potentially resolve any ambiguities in the constructor map using the resolution information.
-- 5. Creates a `SimplifierContext` object using the gathered resolution information (`resolutionMap'`) and resolved constructors (`resolvedConstructors`). This context object holds information used throughout the simplification process.
-- 6. Calls the `runSimplifier` function  with the created context (`ctx`). This function performs the core simplification logic using the context information.
-- 7. Within `runSimplifier`, it calls `convertDefinitions defs` to convert the original definitions to simplified value definitions.
-- 8. Combines the resolved constructors (`resolvedConstructors`) with the simplified definitions (`defs'`) into a new `AST`.
-- 9. Returns the resulting simplified AST wrapped in a `Just` constructor if simplification was successful. Otherwise, it propagates any errors encountered during simplification.
--
-- The `simplifier` function acts as the entry point for the simplification process. It gathers necessary information, creates a simplification context, and delegates the core logic to the `runSimplifier` function. The `convertDefinitions` function is used within `runSimplifier` to prepare the value definitions for further processing.
simplifier :: P.AST -> Either AnalyzeError (AST ())
simplifier (P.AST defs) = do
  resolutionMap' <- gatherResolutions defs
  constructorMap' <- buildVariantInfoMap defs
  resolvedConstructors <- resolveVariantTypes resolutionMap' constructorMap'
  let ctx = SimplifierContext resolutionMap' resolvedConstructors
  runSimplifier ctx <| do
    defs' <- convertDefinitions defs
    return (AST resolvedConstructors defs')
