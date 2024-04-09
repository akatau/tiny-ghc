{-# LANGUAGE FlexibleContexts #-} -- Enables the use of `forall` and other polymorphic kinds in type annotations.

-- This language extension allows for more flexible contexts by enabling the use of polymorphic kinds in type annotations.
-- For instance, with this extension, we can define a type variable that can range over types with a specific kind,
-- such as `t :: * -> *`, which represents a type constructor that takes one type argument and returns another type.
{-# LANGUAGE FlexibleInstances #-} -- Allows for deferring instance resolution until type inference.

-- This language extension allows us to defer the resolution of tinyHaskell typeclass instances until type inference is complete.
-- This can be useful in situations where the type of an expression depends on the types of other expressions,
-- and the typeclass instance for a particular type might not be known until all types have been inferred.
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Allows deriving instances of newtype constraints.

-- This language extension enables deriving instances of newtype constraints for user-defined newtype types.
-- A newtype is a type that introduces a new name for an existing type, without changing its underlying representation.
-- With this extension, we can derive instances of typeclasses like `Eq` and `Show` for our newtype automatically,
-- ensuring they behave as expected when comparing or displaying values of that newtype.
{-# LANGUAGE TupleSections #-} -- Enables named field access in tuples.

-- This language extension allows us to access elements of a tuple using named fields instead of their numerical positions.
-- For instance, with this extension, we can write `person.name` instead of `person._1` to access the name field of a tuple `(String, Int) person`.
-- This can improve code readability and maintainability, especially when dealing with tuples with many elements or meaningful field names.

-- This module defines the type checker for tinyHaskell.
module TypeChecker (typer, TypeError) where

import Control.Monad 
  (
    foldM, -- Folds a monad over a list, accumulating a result in the monad.
    forM,  -- Applies a monadic action to each element of a list, collecting the results.
    unless, -- Executes an action only if a condition is not satisfied.
  )

import Control.Monad.Except 
  (
    Except,  -- Represents a monad that can either succeed with a value or fail with an exception.
    MonadError (throwError), -- Allows throwing exceptions in a monad.
    runExcept,
    liftEither -- Lifts an `Either` value into the monad, discarding the Left constructor.
  )
import Control.Monad.Reader 
  (
    MonadReader (local), -- Allows modifying the environment in a monad.
    ReaderT (..),        -- Reader monad transformer, allows accessing an environment in a monad.
    asks                -- Retrieves the environment from a `ReaderT` monad.
  )
import Control.Monad.State 
  (
    MonadState (get, put), -- Provides state monad functionality for getting and setting state.
    StateT (runStateT)     -- State monad transformer, allows managing state in a monad.
  )
import Data.List (delete, find, nub) -- Functions for manipulating lists.
import qualified Data.Map as Map      -- Functions for working with Maps.
import qualified Data.Set as Set      -- Functions for working with Sets.
import Ourlude                       
import ASTSimplifier 
  (
    AST (..),            -- Abstract syntax tree representation for tinyHaskell expressions.
    BuiltinFunction (..),        -- Built-in functions and operators of tinyHaskell.
    ConstructorInfo (..), -- Information about constructors (data types)
    ConstructorMap,      -- Map of constructor names to their information.
    ConstructorName,    -- Identifier of a constructor.
    SimplifiedExpression (..),            -- Expressions in tinyHaskell.
    HasVariantInfo (..), -- Constraint requiring access to a constructor map.
    Value (..),         -- Value values (integers, strings, booleans).
    Identifier,                -- Names of variables, functions, and constructors.
    Match (..),         -- Patterns for matching in case expressions.
    TypeName,            -- Type names in tinyHaskell.
    TypeArgument,             -- Type variables used for polymorphism.
    ValueIdentifier,             -- Names of values (variables or constructors).
    ValueDefinition (..), -- Definition of a value (variable binding).
    getVariantInfo -- Looks up a constructor in the constructor map, throws an error if not found.
  )
import Types 
  (
    FreeTypeVariables (..),   -- Types without explicit type variables.
    PolyType (..),         -- Type schemes (universally quantified types).
    Type (..),           -- Types in tinyHaskell.
    moreGeneral           -- Checks if a scheme is more general than another.
  )

-- Represents a type of error that can occur during type checking.
data TypeError
  = TypeMismatch Type Type        -- Mismatch between two types.
  | InfiniteType TypeArgument Type      -- Recursive type definition.
  | UnboundName Identifier              -- Reference to an undefined name.
  | NotGeneralEnough PolyType PolyType -- Inferred scheme is not as general as declared one.
  deriving (Show, Eq)

-- Represents a constraint generated during type inference.
-- Provides information about how types are used and is necessary for inferring correct types.
data Constraint
  = SameType Type Type            -- Two types must be equal.
  | ExplicitlyInstantiates Type PolyType -- A type explicitly instantiates a scheme.
  | ImplicitlyInstantations Type (Set.Set TypeArgument) Type -- A type implicitly instantiates another type, given bound variables.
  deriving (Show, Eq)

-- Represents a substitution of type variables for actual types.
newtype Substitution = Substitution (Map.Map TypeArgument Type) deriving (Show, Eq)

instance Semigroup Substitution where
  -- (<>) :: Substitution -> Substitution -> Substitution
  -- Combines two substitutions (Substitution) using the `<>` operator.
  (Substitution s1) <> (Substitution s2) = Substitution (Map.map (subst (Substitution s1)) s2 <> s1)
    -- Applies the following steps:
    -- 1. `subst (Substitution s1)`: This part applies the substitution `s1` (a map of type variable to type mappings)
    --    to each value in the map `s2`. The `subst` function takes a substitution
    --    and a type, and performs the substitution of type variables according to the mappings in the substitution.
    --    The result is a new map where the values have been potentially updated based on the substitutions in `s1`.
    -- 2. `Map.map ... s2 <> s1`: This part combines the two maps.
    --    - `Map.map` applies the function on the left (in this case, the result of `subst (Substitution s1)`) to each value in the map `s2`.
    --    - `<>` (the merge operator for maps) combines the two maps `s1` and the modified `s2` into a single map.
    --    This merging prioritizes mappings from `s1` in case of conflicts (a type variable has mappings in both `s1` and `s2`).
  -- Finally, the result is wrapped in a new `Substitution` constructor.
  -- {-# INLINABLE (<>) #-}
  -- -- This marks the `<>` operator for potential inlining by the compiler.
  -- -- Inlining can improve performance by avoiding function call overhead.

  -- Instance of Monoid for Substitution
instance Monoid Substitution where
  -- mempty :: Monoid m => m
  -- The neutral element for the combination operation (<>).
  -- In the context of Substitution, it represents an empty substitution.
  mempty = Substitution mempty
    -- This creates a `Substitution` value with an empty `Map.Map TypeArgument Type`.
    -- An empty map signifies that no type variables have been substituted yet.
  {-# INLINABLE mempty #-}
  -- This marks the `mempty` function for potential inlining by the compiler.
  -- Inlining can improve performance by avoiding function call overhead for creating an empty substitution.


createSubstitution :: TypeName -> Type -> Substitution
-- Creates a substitution that maps a single type variable to a specific type.
createSubstitution v t = Substitution (Map.singleton v t)
  -- Parameters:
    -- *v* :: TypeName: The name of the type variable to be substituted.
    -- *t* :: Type: The type that will replace the type variable.

  -- Returns:
    -- A `Substitution` value representing a substitution where the type variable `v` is mapped to the type `t`.

  -- Explanation:
  -- This function creates a substitution that binds a single type variable to a specific type.
  -- It uses the `Map.singleton` function from the `Data.Map` module to create a map with one key-value pair.
  -- The key is the `TypeName` `v`, representing the type variable name.
  -- The value is the `Type` `t`, which is the type that will substitute the type variable in subsequent operations.
  -- The resulting `Substitution` constructor holds this single-element map, representing a substitution that only affects the specified type variable.


-- Class Substitutable

class Substitutable a where
  subst :: Substitution -> a -> a
  -- The `subst` function applies a substitution (`Substitution`) to a value of type `a`.

  -- Explanation:
  -- This class declaration defines a type class named `Substitutable`.
  -- A type class specifies a set of functionalities that types can adhere to.
  -- In this case, the `Substitutable` class requires types to provide a way to apply a substitution (`Substitution`) to a value of that type.

  -- Parameters:
    -- *subst* :: Substitution: A substitution object containing mappings from type variables to actual types.
    -- *a* :: The type of the value to which the substitution is applied. This type must be an instance of the `Substitutable` class.

  -- Returns:
    -- A value of type `a` with the type variables substituted according to the provided `Substitution` object.

  -- Constraints:
  -- The `subst` function is expected to perform the necessary operations to replace occurrences of type variables in the value `a` with the corresponding types defined in the `Substitution` object.
  -- The specific implementation of `subst` will depend on the nature of the type `a`.


-- | Instances of the `Substitutable` typeclass for various types used in the type checking process.
--
-- This class allows us to apply a substitution (`Substitution`) to different types and expressions,
-- effectively replacing type variables with their corresponding types.

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  -- | Substitute all elements within a set using the provided substitution.
  --
  -- This function applies the substitution `subst` to each element within the set `Set.Set a`.
  -- It leverages the `Ord a` constraint to ensure elements are ordered consistently
  -- during the substitution process. The `Set.map` function is used to conveniently
  -- apply the substitution to each element.
  subst = subst >>> Set.map

instance Substitutable TypeName where
  -- | Substitute a type name using the provided substitution.
  --
  -- This function looks up the type name `a` in the substitution `Substitution s`.
  -- If the type name is a type variable (`TVar a`), it returns the corresponding type
  -- associated with that variable in the substitution. Otherwise, it returns the original
  -- type name `a` as it wasn't a variable that needs substitution.
  subst (Substitution s) a = case Map.findWithDefault (TVar a) a s of
    TVar tn -> tn  -- If it's a type variable, return the mapped type
    _ -> a        -- Otherwise, keep the original type name

instance Substitutable Type where
  -- | Substitute a type expression using the provided substitution.
  --
  -- This function performs substitution on various type constructors based on their structure:
  --   - Basic types like `IntT`, `StringT`, and `BoolT` remain unchanged.
  --   - Type variables (`TVar a`) are replaced with their corresponding types from the substitution.
  --   - Function types (`t1 :-> t2`) have both the source and target types substituted.
  --   - Custom types (`CustomType name ts`) have their argument types substituted.
  subst sub@(Substitution s) t = case t of
    IntT -> IntT
    StringT -> StringT
    BoolT -> BoolT
    TVar a -> Map.findWithDefault (TVar a) a s  -- Replace type variables
    t1 :-> t2 -> subst sub t1 :-> subst sub t2
    CustomType name ts -> CustomType name (map (subst sub) ts) -- Substitute arguments in custom types

instance Substitutable PolyType where
  -- | Substitute a type scheme using the provided substitution.
  --
  -- This function performs substitution on a type scheme (`PolyType vars t`). It first
  -- creates a new substitution `s'` by removing all scheme variables from the original substitution `s`.
  -- Then, it applies the modified substitution `s'` to the type expression `t` within the scheme.
  subst (Substitution s) (PolyType vars t) =
    let s' = Substitution (foldr Map.delete s vars)  -- Remove scheme vars from substitution
    in PolyType vars (subst s' t)             -- Substitute type expression

instance Substitutable Constraint where
  -- | Substitute a constraint using the provided substitution.
  --
  -- This function applies the substitution `s` to the types within different constraint types:
  --   - `SameType t1 t2`: Substitute both `t1` and `t2`
  --   - `ExplicitlyInstantiates t sc`: Substitute both `t` and the scheme `sc`
  --   - `ImplicitlyInstantations t1 vars t2`: Substitute `t1`, `t2`, and keep the variable set `vars` unchanged
  subst s (SameType t1 t2) = SameType (subst s t1) (subst s t2)
  subst s (ExplicitlyInstantiates t sc) =
    ExplicitlyInstantiates (subst s t) (subst s sc)
  subst s (ImplicitlyInstantations t1 vars t2) =
    ImplicitlyInstantations (subst s t1) (subst s vars) (subst s t2)

-- | Class for types where we can identify important type variables.
--
-- This class, `ActiveTypeVars`, provides a way to determine which type variables are significant
-- within a given type or expression. These important variables are crucial for the type inference process.

class ActiveTypeVars a where
  -- | Extracts the set of active type variables from a value of type `a`.
  --
  -- This function `activeTypeVariables` takes a value `a` and returns a set of `TypeName` representing the type variables
  -- that are considered important or active within that value. The specific meaning of "active" can vary
  -- depending on the type of `a`. The `ActiveTypeVars` class provides a way to define this behavior for
  -- different types used in the type checking process.
  activeTypeVariables :: a -> Set.Set TypeName

instance ActiveTypeVars Constraint where
  -- | Active type variables in a constraint.
  --
  -- This instance defines how to find active type variables within a constraint (`Constraint`). There are
  -- three different cases to consider:
  --   - `SameType t1 t2`: Both `t1` and `t2` can potentially contain important type variables, so we
  --     use `Set.union` to combine the active variables from both types.
  --   - `ExplicitlyInstantiates t sc`: Both the type expression `t` and the scheme `sc` can contain
  --     active variables, so we find the active variables in both using `freeTypeVars` and combine them using
  --     `Set.union`.
  --   - `ImplicitlyInstantations t1 vars t2`: Here, the important variables are those appearing in the
  --     first type `t1` and the free variables from the set `vars` that are implicitly bound on the right
  --     side of the constraint. We use `Set.union` to combine the active variables from `t1` and
  --     `freeTypeVars vars`, and then intersect this set with the active variables in `t2` to keep only the
  --     relevant ones that are used in both `t1` and `t2`.
  activeTypeVariables (SameType t1 t2) = Set.union (freeTypeVars t1) (freeTypeVars t2)
  activeTypeVariables (ExplicitlyInstantiates t sc) = Set.union (freeTypeVars t) (freeTypeVars sc)
  activeTypeVariables (ImplicitlyInstantations t1 vars t2) =
    Set.union (freeTypeVars t1) (Set.intersection (freeTypeVars vars) (freeTypeVars t2))

instance ActiveTypeVars a => ActiveTypeVars [a] where
  -- | Active type variables in a list.
  --
  -- This instance leverages the `ActiveTypeVars` constraint on the element type `a` to determine active
  -- type variables in a list of elements of type `a`. It simply folds over the list using `foldMap` and the
  -- `activeTypeVariables` function to collect the active variables from each element and combine them into a single set.
  activeTypeVariables = foldMap activeTypeVariables



-- | The inference environment used during type inference.
--
-- This data type, `InferenceEnvironment`, represents the environment used when performing type inference.
-- It stores two key components:
--   - `bound`: A set of `TypeName` representing the type variables that are currently bound or in scope
--     during the inference process.
--   - `constructorInfo`: A `ConstructorMap` containing information about constructors used in the program.
data InferenceEnvironment = InferenceEnvironment {
  bound :: Set.Set TypeName,  -- Set of bound type variables
  constructorInfo :: ConstructorMap -- Information about constructors
}


-- | The type for inference contexts.
--
-- This newtype, `Infer a`, represents the type of inference contexts used during type inference.
-- It encapsulates a monadic computation of type `a` that has access to several functionalities:
--   - Reader access to an `InferenceEnvironment` environment, allowing the computation to access the current binding
--     information and constructor details.
--   - State management of an integer,  used for generating unique identifiers during inference.
--   - Error handling using the `Except` monad with a `TypeError` type, enabling the context to throw type errors.
--
-- The `Infer` type provides a convenient way to perform type inference computations with access to the
-- environment, generateFreshName type variable generation, error handling, and other functionalities.
newtype Infer a = Infer (ReaderT InferenceEnvironment (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader InferenceEnvironment, MonadState Int, MonadError TypeError)

instance HasVariantInfo Infer where
  constructorMap = asks constructorInfo

-- | Run the inference context with a specific constructor map.
--
-- This function, `runInfer`, takes an `Infer a` computation and a `ConstructorMap`. It runs the
-- computation within a `ReaderT` monad that provides access to the `InferenceEnvironment` environment containing
-- the constructor map. It then uses `runStateT` to manage the internal state ( for generateFreshName variable
-- generation) and `runExcept` to handle potential type errors. Finally, it extracts the resulting value
-- from the `Either TypeError a` monad.
runInfer :: Infer a -> ConstructorMap -> Either TypeError a
runInfer (Infer m) info =
  runReaderT m (InferenceEnvironment Set.empty info)
    |> (`runStateT` 0)
    |> runExcept
    |> fmap fst

-- | Generate a generateFreshName type variable during inference.
--
-- This function, `generateFreshName`, performs a monadic computation within the `Infer` context. It retrieves the
-- current state using `get`, increments it to generate a unique identifier, and updates the state using
-- `put`. Finally, it returns a new type variable name by prepending "#" to the generated identifier.
generateFreshName :: Infer TypeArgument
generateFreshName =
  Infer <| do
    count <- get
    put (count + 1)
    return ("#" <> show count)

-- | Instantiate a type scheme by replacing parameters with generateFreshName variables.
--
-- This function, `instantiate`, takes a `PolyType` and performs an `Infer` computation. It iterates over
-- the scheme variables using `forM` and generates generateFreshName type variables using `generateFreshName` for each parameter.
-- Then, it constructs a substitution (`sub`) by pairing each scheme variable with its corresponding generateFreshName
-- type variable. Finally, it applies the substitution to the scheme's type expression and returns the
-- instantiated type.
instantiate :: PolyType -> Infer Type
instantiate (PolyType vars t) = do
  newVars <- forM vars (const generateFreshName)
  let sub = foldMap (uncurry createSubstitution) (zip vars (map TVar newVars))
  return (subst sub t)

-- | Generalize a type into a scheme by closing over all unbound variables.
--
-- This function, `generalize`, takes a set of free type variables (`free`) and a type expression (`t`).
-- It calculates the list of all type variables appearing in `t` using `freeTypeVars`. Then, it extracts the list of
-- type variables that are not present in the `free` set (i.e., the unbound variables) using
-- `Set.difference`. Finally, it creates a scheme with these unbound variables as the scheme parameters
-- and the original type expression `t` as the body.
generalize :: Set.Set TypeArgument -> Type -> PolyType
generalize free t =
  let as = Set.toList (Set.difference (freeTypeVars t) free)
   in PolyType as t


-- | Modify the inference context to mark a type variable as bound.
--
-- This function, `bindVariable`, takes a type variable (`a`) and an `Infer a` computation. It uses the
-- `local` function to modify the current `InferenceEnvironment` environment within the computation. The modification
-- involves inserting the type variable `a` into the set of bound type variables (`bound`) in the environment.
-- This essentially makes `a` known within the scope of the computation.
bindVariable :: TypeArgument -> Infer a -> Infer a
bindVariable a = local (\r -> r {bound = Set.insert a (bound r)})

-- | Modify the inference context to mark multiple type variables as bound.
--
-- This function, `bindVariables`, takes a set of type variables (`vars`) and an `Infer a` computation.
-- Similar to `bindVariable`, it uses `local` to modify the environment. However, it uses `Set.union` to combine
-- the existing set of bound variables with the provided set `vars`. This effectively marks all type variables
-- in `vars` as bound within the computation's scope.
bindVariables :: Set.Set TypeArgument -> Infer a -> Infer a
bindVariables vars = local (\r -> r {bound = Set.union (bound r) vars})






-- | Ordered collection of assumptions gathered during type inference.
--
-- This newtype, `Assumptions`, represents an ordered collection of assumptions made about types during
-- the type inference process. Each assumption is a pair of a `Identifier` ( a variable name) and a `Type`
-- representing the assumed type for that name. The `Assumptions` type follows the `Show`, `Semigroup`, and
-- `Monoid` typeclasses, allowing it to be displayed as a string, concatenated with other assumptions,
-- and treated as an empty monoid (having an empty set of assumptions as the identity element).
newtype Assumptions = Assumptions [(Identifier, Type)]
  deriving (Show, Semigroup, Monoid)

-- | Remove an assumption about a specific name.
--
-- This function, `removeAssumption`, takes a `Identifier` (variable name) and an `Assumptions` collection.
-- It filters the assumptions list in `as` to keep only those where the first element (name) is different
-- from the provided `v`. This effectively removes any assumption previously made about the given name.
-- The filtered list is then wrapped back into an `Assumptions` type and returned.
removeAssumption :: Identifier -> Assumptions -> Assumptions
removeAssumption v (Assumptions as) = Assumptions (filter ((/= v) . fst) as)

-- | Create an assumption for a single name-type pair.
--
-- This function, `createAssumption`, takes a `Identifier` and a `Type`. It constructs a new `Assumptions`
-- collection containing a single element: a pair of the provided `v` (name) and the provided `t` (type).
-- This function is useful for creating a new assumption about a specific name and its type.
createAssumption :: Identifier -> Type -> Assumptions
createAssumption v t = Assumptions [(v, t)]

-- | Look up all assumptions associated with a specific name.
--
-- This function, `lookupAssumptions`, takes a `Identifier` and an `Assumptions` collection. It iterates over the
-- assumptions list in `as` and extracts the type (`t`) from each pair where the first element (name)
-- matches the provided `target`. The extracted types are collected into a list and returned. This function
-- allows retrieving all assumed types associated with a particular name.
lookupAssumptions :: Identifier -> Assumptions -> [Type]
lookupAssumptions target (Assumptions as) =
  [t | (v, t) <- as, v == target]

-- | Get the set of all names used in the assumptions collection.
--
-- This function, `assumptionNames`, takes an `Assumptions` collection. It extracts all the first elements
-- (names) from the name-type pairs in the assumptions list `as`. These names are then converted into a set
-- using `Set.fromList` to ensure uniqueness. This function provides a way to retrieve all the names for which
-- assumptions have been made.
assumptionNames :: Assumptions -> Set.Set Identifier
assumptionNames (Assumptions as) = Set.fromList (map fst as)





-- | Get the type scheme associated with a built-in name.
--
-- This function, `builtinScheme`, takes a `BuiltinFunction` value representing a built-in operator or function.
-- It returns a `PolyType` that describes the expected types for the arguments and the return type of that
-- built-in entity. This function provides a way to map built-in names to their corresponding type schemes
-- during type inference.
builtinScheme :: BuiltinFunction -> PolyType
builtinScheme Compose =  -- PolyType for the `compose` function
  PolyType
    ["a", "b", "c"]
    ((TVar "b" :-> TVar "c") :-> (TVar "a" :-> TVar "b") :-> (TVar "a" :-> TVar "c"))
builtinScheme Cash =     -- PolyType for the `cash` function
  PolyType
    ["a", "b"]
    ( (TVar "a" :-> TVar "b") :-> TVar "a" :-> TVar "b"
    )
builtinScheme b =          -- PolyType for other built-in operators
  PolyType [] <| case b of
    Add -> IntT :-> IntT :-> IntT
    Sub -> IntT :-> IntT :-> IntT
    Mul -> IntT :-> IntT :-> IntT
    Div -> IntT :-> IntT :-> IntT
    Concat -> StringT :-> StringT :-> StringT
    Less -> IntT :-> IntT :-> BoolT
    LessEqual -> IntT :-> IntT :-> BoolT
    Greater -> IntT :-> IntT :-> BoolT
    GreaterEqual -> IntT :-> IntT :-> BoolT
    EqualTo -> IntT :-> IntT :-> BoolT
    NotEqualTo -> IntT :-> IntT :-> BoolT
    And -> BoolT :-> BoolT :-> BoolT
    Or -> BoolT :-> BoolT :-> BoolT
    Negate -> IntT :-> IntT
    _ -> error "Already handled"

-- | Get the type of a literal value.
--
-- This function, `littType`, takes a `Value` value representing a literal constant. It returns the
-- corresponding `Type` of that literal. This function is used during type inference to determine the types
-- of literal expressions encountered in the program.
littType :: Value -> Type
littType (IntegerLiteral _) = IntT
littType (StringLiteral _) = StringT
littType (BooleanLiteral _) = BoolT





-- | Infer the type of an expression.
--
-- This function, `inferExpr`, takes an `SimplifiedExpression ()` representing an expression and performs type inference
-- on it. It returns a monadic computation within the `Infer` context that yields a 4-tuple:
--   - `Assumptions`: Assumptions made about variables during inference.
--   - `[Constraint]`: Constraints generated while inferring the type.
--   - `Type`: The inferred type of the expression.
--   - `SimplifiedExpression Type`: The original expression with its inferred type attached.
--
-- This function is the core of the type inference process. It recursively analyzes different expression
-- constructs and performs the necessary type inference steps for each case.
inferExpr :: SimplifiedExpression () -> Infer (Assumptions, [Constraint], Type, SimplifiedExpression Type)
inferExpr expr = case expr of
  -- Error handling: Create a generateFreshName type variable, generate an empty set of assumptions and constraints.
  Error err -> do
    tv <- TVar <$> generateFreshName
    return (mempty, [], tv, Error err)
  -- Value expression: Extract the type based on the literal kind (IntT, StringT, BoolT).
  LiteralExpression litt ->
    let t = littType litt
     in return (mempty, [], t, LiteralExpression litt)
  -- Function application: Infer types for the function and argument expressions,
  -- generate a generateFreshName type variable for the result, and create a constraint ensuring
  -- the argument type matches the expected function parameter type.
  FunctionApplicationExpression e1 e2 -> do
    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2
    tv <- TVar <$> generateFreshName
    let cs' = [SameType t1 (t2 :-> tv)] <> cs1 <> cs2
    return (as1 <> as2, cs', tv, FunctionApplicationExpression e1' e2')
  -- Built-in function: Instantiate the scheme for the built-in function to get its type.
  BuiltinFunction b -> do
    t <- instantiate (builtinScheme b)
    return (mempty, [], t, BuiltinFunction b)
  -- Variable expression: Generate a generateFreshName type variable and make an assumption
  -- about the variable name having that type.
  IdentifierExpr n -> do
    tv <- TVar <$> generateFreshName
    return (createAssumption n tv, [], tv, IdentifierExpr n)
  -- Case expression: Infer the type of the scrutinee expression, then infer the
  -- types and constraints for each case pattern definition, considering the scrutinee
  -- type. Generate additional constraints to ensure all branches have the same return type.
  CaseExpression e pats -> do
    (as1, cs1, t, e') <- inferExpr e
    inferred <- forM pats (inferPatternDef t)  -- Infer for each case pattern
    let pats' = map (\(_, _, _, p) -> p) inferred  -- Extract the patterns
        (as2, cs2) = foldMap (\(a, c, _, _) -> (a, c)) inferred  -- Combine assumptions/constraints
    ret <- TVar <$> generateFreshName  -- Fresh type variable for the return type
    let cs3 = map (\(_, _, branchRet, _) -> SameType ret branchRet) inferred  -- Same return type constraint
    return (as2 <> as1, cs3 <> cs2 <> cs1, ret, CaseExpression e' pats')
  -- Lambda expression: Generate a generateFreshName type variable for the lambda argument,
  -- perform type inference for the body expression with that argument bound, and
  -- create constraints ensuring the inferred type of the body matches the declared
  -- return type of the lambda expression. Remove the assumption about the argument
  -- variable afterwards.
  LambdaExpression n _ e -> do
    a <- generateFreshName
    let tv = TVar a
    (as, cs, t, e') <- bindVariable a (inferExpr e)
    return
      ( removeAssumption n as,
        [SameType t' tv | t' <- lookupAssumptions n as] <> cs,  -- Constraints for argument type
        tv :-> t,
        LambdaExpression n tv e'
      )
  -- Let expression: Infer the type of the body expression, then infer types and
  -- constraints for each definition in the `let` block, considering any assumptions
  -- made in the body expression.
  LetExpression defs e -> do
    (as1, cs1, t, e') <- inferExpr e
    (as2, cs2, defs') <- inferDefs as1 defs
    return (as2, cs1 <> cs2, t, LetExpression defs' e')

-- | Infer the type and constraints for a pattern definition in a case expression.
--
-- This function, `inferPatternDef`, takes the scrutinee's inferred type (`scrutinee`) and a pattern definition
-- represented by a `Match` and an `SimplifiedExpression ()`. It performs type inference for the expression part of the
-- definition considering the scrutinee's type. It returns a monadic computation within the `Infer` context
-- that yields a 4-tuple:
--   - `Assumptions`: Assumptions made or adjusted based on the pattern matching.
--   - `[Constraint]`: Constraints generated during inference for pattern matching.
--   - `Type`: The inferred type for the expression part of the definition.
--   - `(Match, SimplifiedExpression Type)`: The original pattern paired with the inferred type for the expression.
--
-- This function is crucial for handling pattern matching in case expressions. It ensures type compatibility
-- between the scrutinee and the pattern, and generates constraints based on the specific pattern structure.
inferPatternDef :: Type -> (Match, SimplifiedExpression ()) -> Infer (Assumptions, [Constraint], Type, (Match, SimplifiedExpression Type))
inferPatternDef scrutinee (pat, e) = do
  tv <- TVar <$> generateFreshName  -- Fresh type variable for the pattern
  (cs1, valMap, boundSet) <- inspectPattern tv pat  -- Analyze the pattern structure
  (as, cs2, t, e') <- bindVariables boundSet (inferExpr e)  -- Infer for the expression with bound variables
  return
    ( adjustValAssumptions valMap as,
      SameType tv scrutinee : cs1 <> cs2 <> valConstraints valMap as,  -- Combine constraints
      t,
      (pat, e')
    )
  where
    -- Analyze a pattern, returning constraints, value mappings, and bound variables.
    inspectPattern :: Type -> Match -> Infer ([Constraint], Map.Map ValueIdentifier Type, Set.Set TypeArgument)
    inspectPattern scrutinee' pat' = case pat' of
      -- Wildcard pattern: No constraints, empty value map, and empty bound set.
      Wildcard -> return ([], Map.empty, Set.empty)
      -- Value pattern: Constraint to match scrutinee with the literal type, empty value map, and empty bound set.
      LiteralPattern litt -> return ([SameType scrutinee (littType litt)], Map.empty, Set.empty)
      -- Constructor pattern: Generate generateFreshName type variables for sub-patterns,
      -- construct the expected type for the constructor, create a value map
      -- associating pattern variables with their types, and retrieve the constructor's type information.
      ConstructorPattern cstr pats -> do
        patVars <- forM pats (const generateFreshName)
        let patTypes = map TVar patVars
            patType = foldr (:->) scrutinee' patTypes
            valMap = zip pats patTypes |> Map.fromList
        constructor <- constructorType <$> getVariantInfo cstr
        return ([ExplicitlyInstantiates patType constructor], valMap, Set.fromList patVars)

    -- Adjust assumptions based on the value map from the pattern analysis.
    adjustValAssumptions :: Map.Map ValueIdentifier Type -> Assumptions -> Assumptions
    adjustValAssumptions mp as = foldr removeAssumption as (Map.keys mp)

    -- Generate constraints ensuring values extracted from the scrutinee match their assumed types.
    valConstraints :: Map.Map ValueIdentifier Type -> Assumptions -> [Constraint]
    valConstraints mp as =
      foldMap (\(n, t) -> [SameType t t' | t' <- lookupAssumptions n as]) (Map.toList mp)

-- | Infer types for a sequence of value definitions within a `let` expression.
--
-- This function, `inferDefs`, takes the current assumptions (`usageAs`) and a list of `ValueDefinition ()`
-- representing value definitions within a `let` block. It performs type inference for each definition's
-- expression and combines the resulting assumptions, constraints, and typed definitions.
-- It returns a monadic computation within the `Infer` context that yields a 3-tuple:
--   - `Assumptions`: Assumptions made or combined during type inference for definitions.
--   - `[Constraint]`: Constraints generated while inferring types for definitions.
--   - `[ValueDefinition Type]`: The original definitions with their inferred types.
--
-- This function is essential for handling `let` expressions. It ensures type safety by inferring types
-- for each definition's expression and considering the context's existing assumptions.
inferDefs :: Assumptions -> [ValueDefinition ()] -> Infer (Assumptions, [Constraint], [ValueDefinition Type])
inferDefs usageAs defs = do
  together <-
    forM defs <| \(ValueDefinition n declared _ e) -> do  -- Process each definition
      (as, cs, t, e') <- inferExpr e                      -- Infer type for the expression
      let extra = foldMap (\sc -> [ExplicitlyInstantiates t sc]) declared  -- Constraints for declared types
      return (as, extra ++ cs, (n, t), ValueDefinition n declared t e')  -- Combine results

  bound' <- asks bound                                       -- Get current bound type variables
  let as = usageAs <> foldMap (\(x, _, _, _) -> x) together  -- Combine assumptions
      cs = foldMap (\(_, x, _, _) -> x) together            -- Combine constraints
      defs' = map (\(_, _, _, def) -> def) together          -- Extract typed definitions
      usages = map (\(_, _, usage, _) -> usage) together    -- Extract usages of defined names
      -- Function to process a definition usage and update assumptions/constraints
      process (n, t) (as', cs') =
        (removeAssumption n as',  -- Remove assumption about the name
        [ImplicitlyInstantations t' bound' t | t' <- lookupAssumptions n as'] <> cs')  -- Constraints for implicit instantiation

  -- Apply `process` to each usage of a defined name
  let (as', cs') = foldr process (as, cs) usages

  return (as', cs', defs')


-- {-# LANGUAGE OverloadedLists #-}

{- | Constraint Solving -}

-- | Solve a list of type constraints, producing a valid substitution for type variables.
--
-- This function, `solve`, takes a list of `Constraint` representing type constraints and attempts to
-- find a valid substitution that satisfies all constraints. It returns a monadic computation within the
-- `Infer` context that yields a `Substitution` representing the type substitution.
--
-- This function is crucial for resolving the relationships between type variables during type inference.
-- A valid solution ensures all constraints are met, resulting in a consistent type system.
solve :: [Constraint] -> Infer Substitution
solve [] = return mempty  -- Empty list of constraints is trivially satisfied, return empty substitution.
solve constraints = solve' (nextSolvable True constraints)  -- Start solving with remaining constraints.

  where
    -- Function to choose a constraint for solving, considering equality.
    chooseOne :: Eq a => [a] -> [(a, [a])]
    chooseOne as = [(a, bs) | a <- as, let bs = delete a as]  -- Choose one constraint, remove it from the list.

    -- Find the next solvable constraint (if any) using a chosen strategy.
    nextSolvable :: Bool -> [Constraint] -> (Constraint, [Constraint])
    nextSolvable trySorting xs = case find solvable (chooseOne xs) of
      Just c -> c  -- Found a solvable constraint, return it.
      Nothing | trySorting -> nextSolvable False (nub xs)  -- Try a different solving strategy if possible.
      Nothing -> error ("Couldn't find solvable constraint inside of:\n" ++ show xs)  -- No solvable constraint found, error.
      where
        -- Functions to check if a constraint is solvable based on its type.
        solvable (SameType _ _, _) = True  -- Equality constraint is solvable.
        solvable (ExplicitlyInstantiates _ _, _) = True  -- Explicit instantiation constraint is solvable.
        solvable (ImplicitlyInstantations t1 bound' t2, cs) =
          Set.null (Set.intersection (activeTypeVariables cs) (Set.difference (freeTypeVars t2) bound'))  -- Implicit instantiation is solvable if no bound variable conflicts.
            -- Check for conflicts between type variables used in constraints (activeTypeVariables) and
            -- free type variables in the instantiated type (freeTypeVars t2) that are not bound by the definition (bound').

    -- Recursive function to solve remaining constraints after choosing and handling the initial one.
    solve' :: (Constraint, [Constraint]) -> Infer Substitution
    solve' (c, cs) = case c of
      SameType t1 t2 -> do
        su1 <- unify t1 t2  -- Unify the two types, producing a substitution.
        su2 <- solve (map (subst su1) cs)  -- Solve remaining constraints after applying the substitution.
        return (su2 <> su1)  -- Combine the substitution from unification and remaining constraints.
      ImplicitlyInstantations t1 bound' t2 ->
        solve (ExplicitlyInstantiates t1 (generalize bound' t2) : cs)  -- Convert implicit instantiation to explicit one with generalization.
      ExplicitlyInstantiates t sc -> do
        sc' <- instantiate sc  -- Instantiate the scheme to get a concrete type.
        solve (SameType t sc' : cs)  -- Add an equality constraint with the instantiated type and solve remaining constraints.

-- | Unify two type expressions.
--
-- This function, `unify`, takes two `Type` expressions and attempts to find a most general unifier,
-- which is a substitution that makes the two types equivalent. It returns a monadic computation within the
-- `Infer` context that yields a `Substitution` representing the unifier.
--
-- Unification is a core concept in type inference. It allows us to find consistent relationships between
-- type expressions. A successful unification returns a substitution that can be applied to both types to
-- make them equivalent.
unify :: Type -> Type -> Infer Substitution
unify t1 t2 | t1 == t2 = return mempty  -- Already equal, no substitution needed.
             | otherwise = case (t1, t2) of
               (TVar n, t) -> bind n t  -- Bind type variable to the other type expression.
               (t, TVar n) -> bind n t  -- Bind type variable to the other type expression (symmetric case).
               (t1 :-> t2, t3 :-> t4) -> do
                 su1 <- unify t1 t3  -- Unify function argument types.
                 su2 <- unify (subst su1 t2) (subst su1 t4)  -- Unify function return types after applying substitution.
                 return (su2 <> su1)  -- Combine substitutions from both unifications.
               (CustomType name1 ts1, CustomType name2 ts2)
                 | name1 == name2 && length ts1 == length ts2 ->
                   let together = zip ts1 ts2
                       go acc (t1, t2) = do
                         su <- unify (subst acc t1) (subst acc t2)
                         return (su <> acc)
                    in foldM go mempty together  -- Unify corresponding arguments for custom types.
                 | otherwise -> throwError (TypeMismatch t1 t2)  -- Types are not compatible custom types or different lengths.
               _ -> throwError (TypeMismatch t1 t2)  -- Other cases not handled (e.g., unification of function types with non-function types).

-- | Bind a type variable to a type expression.
--
-- This function, `bind`, attempts to bind a `TypeArgument` to a given `Type` expression. It performs checks to
-- ensure the binding does not lead to an infinite type (a type variable appearing within its own definition).
-- It returns a monadic computation within the `Infer` context that yields a `Substitution` representing the binding.
--
-- LambdaBinding is a fundamental operation during unification. It allows us to replace type variables with concrete
-- types, potentially leading to successful unification.
bind :: TypeArgument -> Type -> Infer Substitution
bind a t
  | t == TVar a = return mempty  -- LambdaBinding a variable to itself, no substitution needed.
  | Set.member a (freeTypeVars t) = throwError (InfiniteType a t)  -- Infinite type error (variable in its own definition).
  | otherwise = return (createSubstitution a t)  -- Create a substitution for the variable with the given type.






-- {-# LANGUAGE OverloadedLists #-}

{- | Type Annotation -}

-- | Type Annotations - Context and Computation

-- | Represents the type information available during type inference.
--
-- This data type, `TypingContext`, defines the context for type annotation. It stores:
--   - `typerVars`: A set of `TypeArgument` representing type variables bound within the current scope.
--   - `typerSub`: A `Substitution` representing the current substitution for type variables.
--
-- This context is crucial for keeping track of available type variables and their substitutions
-- during the type inference process.
data TypingContext = TypingContext
  { typerVars :: Set.Set TypeArgument  -- Bound type variables
  , typerSub :: Substitution             -- Current substitution
  }

-- | Type Annotations - Monad for Type Inference

-- | The monad type for type inference computations.
--
-- This newtype, `TypeChecker a`, defines a monad specifically for type inference tasks. It utilizes the
-- `ReaderT` monad transformer to access the `TypingContext` context and the `Except` monad transformer
-- to handle potential `TypeError` exceptions.
--
-- This monad provides a structured way to perform type inference operations while managing context
-- and error handling.
newtype TypeChecker a = TypeChecker (ReaderT TypingContext (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadReader TypingContext, MonadError TypeError)

-- | Run a type inference computation with a specific substitution.
--
-- This function, `runTyper`, takes a `TypeChecker a` computation and a `Substitution` representing the initial substitution.
-- It runs the computation using the `ReaderT` and `Except` monad transformers, providing the context and
-- handling any potential `TypeError` exceptions. It returns an `Either TypeError a` representing the
-- result of the computation, either a value of type `a` or a `TypeError`.
runTyper :: TypeChecker a -> Substitution -> Either TypeError a
runTyper (TypeChecker r) sub = runReaderT r (TypingContext Set.empty sub) |> runExcept

-- | Introduce new type variables for a type inference computation.
--
-- This function, `withTyperNames`, takes a list of `TypeArgument` representing new type variables and a
-- `TypeChecker a` computation. It creates a new context where these type variables are added to the existing
-- bound variables. It uses the `local` function to modify the `TypingContext` context within the computation.
withTyperNames :: [TypeArgument] -> TypeChecker a -> TypeChecker a
withTyperNames vars =
  let addTo = Set.union (Set.fromList vars)  -- Create a set of new type variables
   in local (\r -> r {typerVars = addTo (typerVars r)})  -- Update the context with new variables

-- | Get the scheme for a type expression within the current typing context.
--
-- This function, `schemeFor`, takes a `Type` expression and uses the `TypeChecker` monad to access the
-- current `TypingContext` context. It retrieves the set of bound type variables (`typerVars`) and the
-- current substitution (`typerSub`). It then applies `generalize` with these values to the substituted
-- type expression, obtaining the most general scheme for the given type within the current context.
schemeFor :: Type -> TypeChecker PolyType
schemeFor t = do
  typerVars' <- asks typerVars  -- Access bound type variables from the context.
  typerSub' <- asks typerSub   -- Access current substitution from the context.
  return (generalize typerVars' (subst typerSub' t))  -- Get the most general scheme after substitution.

-- | Type Inference and Checking

-- | Assign types to an expression.
--
-- This function, `typeExpression`, takes an `SimplifiedExpression Type` representing an expression with an optional type annotation
-- and returns a `TypeChecker (SimplifiedExpression PolyType)` computation. It performs type inference for the expression based on
-- its structure and the provided type annotation (if any). It returns a computation that yields an `SimplifiedExpression PolyType`
-- representing the inferred scheme for the expression.
--
-- This function is the core of the type inference process. It handles various expression types, including
-- literals, variables, function applications, lambda expressions, case expressions, and let expressions.
typeExpression :: SimplifiedExpression Type -> TypeChecker (SimplifiedExpression PolyType)
typeExpression expr = case expr of
  Error err -> return (Error err)  -- Return the error if the expression is already marked as erroneous.
  LiteralExpression litt -> return (LiteralExpression litt)  -- No inference needed for literals.
  IdentifierExpr n -> return (IdentifierExpr n)  -- No inference needed for simple variable names.
  BuiltinFunction b -> return (BuiltinFunction b)  -- No inference needed for built-in functions (assumed to have fixed types).
  FunctionApplicationExpression e1 e2 -> FunctionApplicationExpression <$> typeExpression e1 <*> typeExpression e2  -- Infer types for function and argument in an application.
  LambdaExpression n t e -> do
    sc@(PolyType names _) <- schemeFor t  -- Get the scheme for the lambda's parameter type from the context.
    e' <- withTyperNames names (typeExpression e)  -- Infer type for the body with generateFreshName type variables for the parameters.
    return (LambdaExpression n sc e')  -- Create a lambda expression with inferred scheme and typed body.
  CaseExpression e patDefs -> CaseExpression <$> typeExpression e <*> forM patDefs typePatternDef  -- Infer type for the scrutinee and each pattern definition.
  LetExpression defs e -> LetExpression <$> typeDefinitions defs <*> typeExpression e  -- Infer types for definitions and the body of a let expression.

-- | Assign types to a pattern definition (helper for `typeExpression`).
--
-- This function, `typePatternDef`, takes a pair of `Match` and `SimplifiedExpression Type` representing a pattern definition
-- and returns a `TypeChecker (Match, SimplifiedExpression PolyType)` computation. It performs type inference for the expression part
-- of the definition. It returns a computation that yields a pair of `Match` and `SimplifiedExpression PolyType` representing
-- the original pattern and the inferred scheme for the expression.
typePatternDef :: (Match, SimplifiedExpression Type) -> TypeChecker (Match, SimplifiedExpression PolyType)
typePatternDef (pat, expr) = (pat,) <$> typeExpression expr  -- Infer type for the expression in the pattern definition.

-- | Assign types to a series of value definitions.
--
-- This function, `typeDefinitions`, takes a list of `ValueDefinition Type` representing value definitions
-- and returns a `TypeChecker [ValueDefinition PolyType]` computation. It performs type inference for each definition's
-- expression and checks the compatibility of declared types (if any). It returns a computation that yields a
-- list of `ValueDefinition PolyType` representing the definitions with inferred schemes.
--
-- This function ensures type safety within let expressions by inferring types for expressions and checking
-- against declared types.
typeDefinitions :: [ValueDefinition Type] -> TypeChecker [ValueDefinition PolyType]
typeDefinitions defs =
  forM defs <| \(ValueDefinition name ann t e) -> do
    sc <- schemeFor t  -- Get the scheme for the declared type (if any) from the context.
    e' <- typeExpression e   -- Infer type for the expression in the definition.
    case ann of
      Just d | not (moreGeneral sc d) -> throwError (NotGeneralEnough sc d)  -- Check if declared type is more general than inferred.
      _ -> return ()  -- No declared type, nothing to check.
    return (ValueDefinition name ann sc e')  -- Create a typed definition with inferred scheme.

-- | Infer and Check Types for Value Definitions

-- | Infer and check types for a series of value definitions.
--
-- This function, `inferTypes`, takes a list of `ValueDefinition ()` representing value definitions without
-- type annotations and returns an `Infer [ValueDefinition PolyType]` computation. It performs the following steps:
--
-- 1. **Get all constructor schemes:** It retrieves a map of all constructor names to their corresponding
--    schemes using the `allConstructors` function (assumed to be defined elsewhere).
-- 2. **Infer assumptions and constraints:** Calls `inferDefs` with an empty initial assumption set and the
--    list of definitions. This function performs type inference for each definition and generates a set of
--    assumptions (inferred types for unbound variables) and a list of constraints representing relationships
--    between types. It also returns a list of typed definitions (`ValueDefinition PolyType`).
-- 3. **Check for unbound names:** It calculates the difference between assumed variable names and the set of
--    constructor names. If there are any unbound names (variables used without being defined as constructors),
--    it throws a `UnboundName` error with the name of the first unbound variable.
-- 4. **Generate explicit instantiations:** It iterates through the map of constructors and their schemes. For
--    each constructor, it checks if there is an assumption for its name. If an assumption exists, it creates
--    an `ExplicitlyInstantiates` constraint, forcing the constructor's type variable to be instantiated with
--    the inferred type from the assumption.
-- 5. **Solve constraints:** It combines the generated explicit instantiation constraints with the constraints
--    obtained from `inferDefs`. It then calls `solve` to find a most general unifier that satisfies all
--    constraints. This ensures consistency in the inferred types.
-- 6. **Run type definitions with substitution:** It uses `liftEither` to handle potential `TypeError` exceptions
--    during type checking. It runs the `typeDefinitions` function with the list of typed definitions and the
--    substitution obtained from solving the constraints. This performs type checking and finalizes the typed
--    definitions.
--
-- This function is the core of type inference for let expressions. It ensures type safety by inferring types
-- for expressions, checking for unbound names, and enforcing consistency through constraint solving.

inferTypes :: [ValueDefinition ()] -> Infer [ValueDefinition PolyType]
inferTypes defs = do
  constructors <- allConstructors
  (as, cs, defs') <- inferDefs mempty defs
  let unbound = Set.difference (assumptionNames as) (Map.keysSet constructors)
  unless (Set.null unbound) (throwError (UnboundName (Set.elemAt 0 unbound)))
  let cs' = [ExplicitlyInstantiates t s | (x, s) <- Map.toList constructors, t <- lookupAssumptions x as]
  sub <- solve (cs' <> cs)
  liftEither <| runTyper (typeDefinitions defs') sub
  where
    allConstructors :: Infer (Map.Map ConstructorName PolyType)
    allConstructors = Map.map constructorType <$> constructorMap

-- | Run the Type Checker on an AST

-- | Run the type checker on a given AST, producing just the value definitions, annotated.
--
-- This function, `typer`, takes an `AST ()` representing an abstract syntax tree and returns an `Either TypeError`
-- (AST PolyType)` computation. It performs the following steps:
--
-- 1. **Infer types for definitions:** It calls `inferTypes` with the list of value definitions from the AST.
--    This function infers types for expressions within definitions, checks for type safety, and ensures
--    consistency through constraint solving.
-- 2. **Run inference:** It uses `runInfer` to execute the type inference computation obtained from `inferTypes`.
--    This handles potential `TypeError` exceptions that might occur during inference.
-- 3. **Update AST with inferred schemes:** If type inference succeeds, it applies the inferred schemes to the
--    original AST using the `(<$>)` operator. This results in an `AST PolyType` where the value definitions
--    have their inferred types attached.
--
-- This function is the entry point for type checking an entire AST. It relies on `inferTypes` to perform the
-- core type inference and constraint solving for definitions, ensuring a well-typed program.

typer :: AST () -> Either TypeError (AST PolyType)
typer (AST info defs) = AST info <$> runInfer (inferTypes defs) info
