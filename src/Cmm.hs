{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Cmm
Description : Command Line Interface for the tinyGHC compiler.
Copyright   : (c) Amr Almorsi, 2024
License     : BSD3
Maintainer : amr.saleh@ejust.edu.eg
Stability   : experimental
Portability : POSIX

This module defines the `Cmm` intermediate representation (IR) used in the compilation process
between Spineless Tagless G-machine (STG) and C.

-}

-- | Rationale for Cmm

-- The primary reason for introducing Cmm is to simplify code generation. While generating C directly from STG is
-- possible, it leads to more complex code. This complexity arises from mixing two concerns:
--
-- 1. **Translating STG semantics into static information:** This involves determining details like the types of
--    values that need allocation and the number of allocations required.
-- 2. **Ordering that information into actual C code:** This involves generating C statements that utilize the
--    static information derived from STG semantics.

-- Directly generating C from STG often mixes these concerns together. Separating them into a dedicated Cmm
-- intermediate representation offers several advantages:

-- * **Simplifies both stages:** By separating concerns, both the translation of STG semantics into static
--   information and the generation of C code become simpler and easier to reason about.
-- * **Improves C code quality:** Having a separate Cmm stage allows for easier generation of well-structured
--   imperative statements from STG. These statements can then be further analyzed and optimized to produce
--   more efficient C code.
module Cmm ( Cmm (..),
            BuiltinFunction2 (..),
            FunctionBody (..),
            CodeBlock (..),
            BuiltinFunction1 (..),
            UpdateType (..),
            IRInstruction (..),
            ArgumentInfo (..),
            FunctionName (..),
            LambdaFunction (..),
            Location (..),
            ClosureType(..),
            SubFunctionIndex,
            MemoryAllocation (..),
            cmm,
          )
where

-- | Imports for Cmm code generation
import Ourlude  -- Assuming this module provides helper functions or types related to code generation
import STG (  -- Imports from the STG module ( related to the source language representation)
    CaseBranches (..),
    LambdaExpression (..),
    PrimitiveValue (PrimInt, PrimString),
    ValueIdentifier,
    UpdateableFlag (..),
    LambdaBinding (..),
    BoxedValueType (IntBox, StringBox),
    Atom (..),
    ConstructorTag,
    STG (..),
    BuiltinFunction (..),
    SimplifiedExpression (..)
  )
import Control.Monad (forM)
import Control.Monad.State (MonadState, State, gets, modify', runState)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (MonadReader (local), ReaderT (..), asks, filterM)
import Data.Maybe (fromMaybe, maybeToList)



-- | Types for Representing LambdaFunction Names

-- | Type alias for a plain function name (String)
type PlainFunctionName = String

-- | Data type representing a name that can be assigned to a function
data FunctionName
  = -- | PlainFunction constructor for a standard function name
    PlainFunction PlainFunctionName
  | -- | LambdaCaseFunction constructor for names used within case expressions
    --
    -- This constructor is used to differentiate between alternatives in a
    -- nested case expression scenario. An `SubFunctionIndex` is included to provide
    -- uniqueness within a specific case expression context.
    LambdaCaseFunction SubFunctionIndex
  | -- | Entry constructor for the entry function name
    Entry
  deriving (Show, Eq)

-- | Type alias for an index used within `LambdaCaseFunction` constructor
type SubFunctionIndex = Int

-- | Types for Representing Variable VariableStorage and Types

-- | Data type representing the storage location for a variable within Cmm
data VariableStorage
  = -- | LocalStorage constructor for variables stored locally within closures
    LocalStorage VariableType
  | -- | GlobalStorage constructor for variables that are global functions
    --
    -- This storage type indicates that the variable refers to a function defined at the global scope.
    -- The `SubFunctionIndex` provides an identifier for the global function.
    GlobalStorage SubFunctionIndex
  | -- | CAFStorage constructor for top-level updatable values (Constants, Abstracts, and Functions)
    --
    -- This storage type is used for variables that represent top-level updatable values.
    -- Note that this category includes both constant and abstract function allocations (CAFs)
    -- and shares indices with global functions. The `SubFunctionIndex` provides an identifier for the updatable value.
    CAFStorage SubFunctionIndex
  deriving (Show, Eq)

-- | Data type representing the type of a variable in Cmm
data VariableType
  = -- | PointerVariable constructor for pointer variables
    PointerVariable
  | -- | IntegerVariable constructor for integer variables (64-bit)
    IntegerVariable
  | -- | StringVariable constructor for string variables
    StringVariable
  deriving (Show, Eq)

-- | Data type representing a location within Cmm
data Location
  = -- | IntRegister constructor for the integer register
    --
    -- This constructor represents the integer register available for computations.
    IntRegister
  | -- | StringRegister constructor for the string register
    --
    -- This constructor represents the string register available for computations.
    StringRegister
  | -- | PrimIntLocation constructor for primitive integer values
    --
    -- This constructor represents the location of a primitive integer value directly embedded within the code.
    PrimIntLocation Int
  | -- | PrimStringLocation constructor for primitive string values
    --
    -- This constructor represents the location of a primitive string value directly embedded within the code.
    PrimStringLocation String
  | -- | Bound constructor for variables bound within a closure
    Bound SubFunctionIndex
  |  -- This constructor represents a variable that is captured (bound) by a closure. The `SubFunctionIndex` specifies the position (nth binding) within the closure.
      -- * -- | BoundInt constructor for bound integer variables within a closure (uses Bound)
    BoundInt SubFunctionIndex
  |    -- * -- | BoundString constructor for bound string variables within a closure (uses Bound)
    BoundString SubFunctionIndex
  | -- | Buried constructor for dead pointers within case expressions
    --
    -- This constructor represents a pointer variable that is no longer live within a specific branch of a case expression. The `SubFunctionIndex` provides an identifier for the dead pointer.
    Buried SubFunctionIndex
  |    -- * -- | BuriedInt constructor for dead integer variables within case expressions (uses Buried)
    BuriedInt SubFunctionIndex
  |    -- * -- | BuriedString constructor for dead string variables within case expressions (uses Buried)
    BuriedString SubFunctionIndex
  |  -- | Argument constructor for arguments passed as pointers on the stack
    --
    -- This constructor represents an argument passed to a function. The `SubFunctionIndex` specifies the position (nth argument) of the pointer argument on the stack frame.
    Argument SubFunctionIndex
  | -- | ConstructorArgument constructor for constructor arguments
    ConstructorArgument SubFunctionIndex
    -- This constructor represents an argument passed to a constructor during pattern matching. The `SubFunctionIndex` specifies the position (nth argument) of the constructor argument.
  | -- | Global constructor for global functions
    --
    -- This constructor represents a reference to a global function defined at the top level. The `SubFunctionIndex` identifies the specific global function.
    Global SubFunctionIndex
  | -- | CAF constructor for top-level updatable values
    --
    -- This constructor represents a reference to a top-level updatable value, including constants, abstracts (unevaluated functions), and functions. The `SubFunctionIndex` identifies the updatable value.
    CAF SubFunctionIndex
  | -- | Allocated constructor for allocated closures
    --
    -- This constructor represents a reference to a closure that has been allocated during code generation. The `SubFunctionIndex` specifies the index of the sub-function within the closure (potentially for nested closures).
    Allocated SubFunctionIndex
  | -- | CurrentNode constructor for the current function
    --
    -- This constructor represents a reference to the currently executing function.
    CurrentNode

  deriving (Show, Ord, Eq)

-- | LambdaFunction to determine the variable type based on its location

getLocationType :: Location -> VariableType
getLocationType = \case
  -- Locations for strings
    BoundString _ -> StringVariable
    BuriedString _ -> StringVariable
    StringRegister -> StringVariable
    PrimStringLocation _ -> StringVariable
  -- Locations for integers
    BoundInt _ -> IntegerVariable
    BuriedInt _ -> IntegerVariable
    IntRegister -> IntegerVariable
    PrimIntLocation _ -> IntegerVariable
  -- Locations for pointers
    Argument _ -> PointerVariable
    ConstructorArgument _ -> PointerVariable
    Bound _ -> PointerVariable
    Global _ -> PointerVariable
    CAF _ -> PointerVariable
    Allocated _ -> PointerVariable
    Buried _ -> PointerVariable
    CurrentNode -> PointerVariable
  -- This case shouldn't happen (exhaustiveness check)
  -- _ -> error "getLocationType: unexpected location type"


-- | Types for Direct Updates and Built-in Functions

-- | Data type representing the type of update for direct assignments
data UpdateType
  = -- | IntUpdate constructor for updating an integer value
    IntUpdate
  | -- | StringUpdate constructor for updating a string value
    StringUpdate
  deriving (Show)

-- | Data type representing built-in functions taking two arguments

data BuiltinFunction2
  = -- | Add2 constructor for addition (+). The result is stored in the integer register.
    Add2
  | -- | Sub2 constructor for subtraction (-). The result is stored in the integer register.
    Sub2
  | -- | Mul2 constructor for multiplication (*). The result is stored in the integer register.
    Mul2
  | -- | Div2 constructor for division (/). The result is stored in the integer register.
    Div2
  | -- | Less2 constructor for less than comparison (<). The result (0 or 1) is stored in the integer register.
    Less2
  | -- | LessEqual2 constructor for less than or equal comparison (<=). The result (0 or 1) is stored in the integer register.
    LessEqual2
  | -- | Greater2 constructor for greater than comparison (>). The result (0 or 1) is stored in the integer register.
    Greater2
  | -- | GreaterEqual2 constructor for greater than or equal comparison (>=). The result (0 or 1) is stored in the integer register.
    GreaterEqual2
  | -- | EqualTo2 constructor for equality comparison (==). The result (0 or 1) is stored in the integer register.
    EqualTo2
  | -- | NotEqualTo2 constructor for inequality comparison (/=). The result (0 or 1) is stored in the integer register.
    NotEqualTo2
  | -- | Concat2 constructor for string concatenation (++). The result is stored in the string register.
    Concat2
  deriving (Show)

-- | Data type representing built-in functions taking a single argument

data BuiltinFunction1
  = -- | PrintInt1 constructor for printing an integer value
    PrintInt1
  | -- | PrintString1 constructor for printing a string value
    PrintString1
  | -- | Negate1 constructor for negation (-). The result is stored in the integer register.
    Negate1
  deriving (Show)


-- | Data type representing an instruction in the Cmm intermediate representation (IR)
data IRInstruction
  = -- | StoreInt instruction for storing an integer value in the integer register
    StoreInt Location
  | -- | StoreString instruction for storing a string literal in the string register
    StoreString Location
  | -- | StoreTag instruction for storing a tag value in the tag register
    StoreTag  ConstructorTag
  | -- | StoreConstructorArgCount instruction for storing the number of constructor arguments
    --
    -- This instruction stores the number of constructor arguments being passed to a constructor during pattern matching.
    StoreConstructorArgCount Int
  | -- | PopExcessConstructorArgs instruction for discarding unused constructor arguments
    --
    -- This instruction removes any extra constructor arguments from the stack after pattern matching.
    PopExcessConstructorArgs
  | -- | Enter instruction for transferring control to a new code location
    --
    -- This instruction jumps to the code stored at the specified `Location`. The comment clarifies that the location must contain valid code (not data) for this instruction to be valid.
    Enter Location
  | -- | EnterCaseContinuation instruction for entering the code for a case expression continuation
    --
    -- This instruction yields control to the code for the relevant branch of a case expression, based on the value produced earlier.
      EnterCaseContinuation
  | -- | PrintError instruction for printing an error message
    PrintError String
  | -- | BuiltinFunction2 instruction for applying a built-in function with two arguments
    BuiltinFunction2 BuiltinFunction2 Location Location
      -- | `BuiltinFunction2`: The specific built-in function to apply (e.g., addition, comparison)
      -- | `Location`: Location of the first argument
      -- | `Location`: Location of the second argument
  | -- | BuiltinFunction1 instruction for applying a built-in function with one argument
    BuiltinFunction1 BuiltinFunction1 Location
      -- | `BuiltinFunction1`: The specific built-in function to apply (e.g., printing, negation)
      -- | `Location`: Location of the argument
  | -- | Exit instruction for terminating the program execution
    Exit
  | -- | PushSA instruction for pushing a pointer argument onto the stack
    PushSA Location
  | -- | PushConstructorArg instruction for pushing a constructor argument onto the stack
    PushConstructorArg Location
  | -- | PushCaseContinuation instruction for pushing a case continuation onto the stack
    --
    -- This instruction pushes the code for a specific subfunction containing a case expression onto the stack. The `SubFunctionIndex` specifies the index of the subfunction.
    PushCaseContinuation SubFunctionIndex
  | -- | Bury instruction for hiding a pointer variable within a case expression branch
    Bury Location
  | -- | BuryInt instruction for hiding an integer variable within a case expression branch
    BuryInt Location
  | -- | BuryString instruction for hiding a string variable within a case expression branch
    BuryString Location
  | -- | AllocTable instruction for allocating a table for a function
    AllocTable SubFunctionIndex
      -- | `SubFunctionIndex`: The index of the subfunction within the enclosing function, and also an identifier for the allocated table.
  | -- | AllocPointer instruction for allocating a pointer on the heap
    AllocPointer Location
  | -- | AllocBlankPointer instruction for allocating empty space for a pointer
    --
    -- This instruction allocates space for a pointer, which is necessary for closures to store relocation information for garbage collection.
    AllocBlankPointer
  | -- | AllocInt instruction for allocating an integer on the heap
    AllocInt Location
  | -- | AllocString instruction for allocating a string on the heap
    AllocString Location
  | -- | PushUpdate instruction for pushing an update frame onto the stack
    PushUpdate
  | -- | CreateCAFClosure instruction for creating the dynamic heap part of a Constant Abstract LambdaFunction (CAF)
    CreateCAFClosure SubFunctionIndex
      -- | `SubFunctionIndex`: The index of the CAF that needs its dynamic heap part created.
  deriving (Show)

-- | Data type representing memory allocation information for expressions

data MemoryAllocation = MemoryAllocation
  { -- | tablesAllocated :: Int
    --   The number of tables allocated for closures within the expression
    tablesAllocated :: Int,
   -- | pointersAllocated :: Int
    --   The number of pointers allocated within closures for the expression
    pointersAllocated :: Int,
   -- | intsAllocated :: Int
    --   The number of integer variables allocated within closures for the expression
    intsAllocated :: Int,
   -- | stringsAllocated :: Int
    --   The number of string pointers allocated within closures for the expression
    stringsAllocated :: Int
  }
  deriving (Show, Eq)

-- | Instance of Semigroup for MemoryAllocation
instance Semigroup MemoryAllocation where
  MemoryAllocation t p i s <> MemoryAllocation t' p' i' s' =
    MemoryAllocation (t + t') (p + p') (i + i') (s + s')
  -- This combines the allocation information from two expressions.

-- | Instance of Monoid for MemoryAllocation
instance Monoid MemoryAllocation where
  mempty = MemoryAllocation 0 0 0 0
  -- The empty allocation represents no memory usage.

-- | Type representing the number of constructor arguments passed to a body
type ConstructorArgs = Int
  -- | This type represents the number of constructor arguments passed to the body of a function or a case branch.
  -- | A value of 0 is used for convenience when the body is not associated with a case branch (where constructor arguments are irrelevant).

-- | Data type representing the body of a function or a case branch
data CodeBlock = CodeBlock MemoryAllocation ConstructorArgs [IRInstruction]
  deriving (Show)
  -- | `MemoryAllocation`: Memory allocation information for the body's expressions.
  -- | `ConstructorArgs`: The number of constructor arguments passed to the body (0 for non-case branches).
  -- | `[IRInstruction]`: The list of instructions that make up the body's code.

-- | Instance of Semigroup for CodeBlock
instance Semigroup CodeBlock where
  CodeBlock alloc1 count1 instrs1 <> CodeBlock alloc2 count2 instrs2 =
    CodeBlock (alloc1 <> alloc2) (count1 + count2) (instrs1 <> instrs2)
  -- This combines two bodies by merging their allocation information, adding their constructor argument counts, and concatenating their instruction lists.

-- | Instance of Monoid for CodeBlock
instance Monoid CodeBlock where
  mempty = CodeBlock mempty 0 mempty
  -- The empty body represents no instructions, no constructor arguments, and no memory allocation.

-- | Data type representing information about arguments used in a function context

data ArgumentInfo = ArgumentInfo
  { -- | boundPointers :: Int
    --   The number of bound pointer arguments used in the function.
    boundPointers :: Int,
   -- | boundInts :: Int
    --   The number of bound integer arguments used in the function.
    boundInts :: Int,
   -- | boundStrings :: Int
    --   The number of bound string arguments used in the function.
    boundStrings :: Int
  }
  deriving (Eq, Ord, Show)

-- | Data type representing the body of a function in the Cmm intermediate representation

data FunctionBody
  = -- | IntCaseBody constructor for integer case expressions
    --
    -- This constructor represents a case expression branching on an integer value.
    -- It takes a list of pairs, where each pair represents:
    --   * The integer literal for the case branch.
    --   * The body of code for that specific case branch.
    -- The final `CodeBlock` argument represents the "else" case, which is executed if none of the literal matches in the list apply.
    IntCaseBody [(Int, CodeBlock)] CodeBlock
  | -- | StringCaseBody constructor for string case expressions
    --
    -- This constructor represents a case expression branching on a string value. Similar to `IntCaseBody`, it takes a list of pairs for each case branch and an "else" body.
    StringCaseBody [(String, CodeBlock)] CodeBlock
  | -- | TagCaseBody constructor for tag case expressions
    --
    -- This constructor represents a case expression branching on a tag value. Similar to the previous cases, it uses a list of pairs for specific tag matches and an "else" body.
    TagCaseBody [( ConstructorTag, CodeBlock)] CodeBlock
  | -- | ContinuationBody constructor for continuation function bodies
    --
    -- This constructor represents the body of a function defined within a case expression (continuation). Continuation bodies differ from normal bodies in how they handle bound arguments, which are considered "buried" within the case expression.
    -- It takes a `UpdateType` to indicate the type of update (integer or string) and a `CodeBlock` representing the continuation code.
    ContinuationBody UpdateType CodeBlock
  | -- | NormalBody constructor for normal function bodies
    --
    -- This constructor represents the regular body of a function, containing its instructions.
    NormalBody CodeBlock
  deriving (Show)

-- | Data type representing the type of a closure

data ClosureType
  = -- | DynamicClosure constructor for dynamic closures
    DynamicClosure
  | -- | GlobalClosure constructor for global closures
    --
    -- This constructor represents a closure referencing a global function. The `SubFunctionIndex` identifies the specific global function.
    GlobalClosure SubFunctionIndex
  | -- | CAFClosure constructor for Constant Abstract LambdaFunction (CAF) closures
    --
    -- This constructor represents a closure associated with a CAF. The `SubFunctionIndex` identifies the specific CAF.
    CAFClosure SubFunctionIndex
  deriving (Show)


-- | Data type representing a function in the Cmm intermediate representation (IR)
data LambdaFunction = LambdaFunction
  { -- | functionName :: FunctionName
    --   The name of the function ( derived from the source code).
    functionName :: FunctionName,
    -- | argumentsCount :: Int
    --   The number of pointer arguments the function takes.
    --   Since primitives (integers and strings) are passed by value in Cmm, this field only counts pointer arguments.
    argumentsCount :: Int,
   -- | boundArguments :: ArgumentInfo
    --   Information about the function's bound arguments (pointers, integers, strings).
    --   This information is used for static analysis, garbage collection, and understanding how arguments are passed to continuations in case expressions.
    --   * If the function is a case function, `boundArguments` represents "buried arguments" not stored with the continuation but passed on a stack.
    boundArguments :: ArgumentInfo,
   -- | closureType :: ClosureType
    --   The type of closure associated with the function.
    --   This information is crucial for generating the appropriate closure implementation in C code.
    --   * `DynamicClosure`: A dynamically allocated closure created during function evaluation.
    --   * `GlobalClosure`: A closure referencing a global function (identified by its index).
    --   * `CAFClosure`: A closure associated with a Constant Abstract LambdaFunction (CAF) (identified by its index).
    closureType :: ClosureType,
   -- | body :: FunctionBody
    --   The body of the function, represented by the `FunctionBody` data type.
    --   This specifies the function's behavior using instructions, case expressions, or other constructs.
    body :: FunctionBody,
   -- | subFunctions :: [LambdaFunction]
    --   The list of subfunctions defined within this function (nested functions).
    --   This allows for representing nested function definitions in the IR.
    subFunctions :: [LambdaFunction]
  }
  deriving (Show)

-- | Data type representing a Cmm program in its Abstract Syntax Tree (AST) form
data Cmm = Cmm [LambdaFunction] LambdaFunction
  deriving (Show)
  -- | This data type represents a Cmm program after parsing and initial analysis.

  -- | `[LambdaFunction]`: The list of functions defined within the Cmm program.
  -- | `LambdaFunction`: The designated entry function for the program execution.

-- | Data type representing the context used during Cmm code generation
data CompilationContext = CompilationContext
  { -- | storages :: Map.Map ValueIdentifier VariableStorage
    --   A map from variable names (`ValueIdentifier`) to their corresponding storage locations (`VariableStorage`).
    --   This information is crucial for determining how variables are accessed and manipulated in the generated C code.
    storages :: Map.Map ValueIdentifier VariableStorage,
   -- | locations :: Map.Map ValueIdentifier Location
    --   A map from variable names (`ValueIdentifier`) to their corresponding code locations (`Location`) in the generated C code.
    --   This map is used for referencing variables and expressions within the generated C statements.
    locations :: Map.Map ValueIdentifier Location
  }
  deriving (Show)

-- | The default context to use at the beginning of code generation
startingContext :: CompilationContext
startingContext = CompilationContext mempty mempty

-- | Data type representing the state information maintained within the context
data ContextState = ContextState
  { -- | freshIndex :: SubFunctionIndex
    --   An index used for generating generateFreshName variable names during code generation.
    --   This ensures unique variable names are created to avoid conflicts within the generated C code.
    freshIndex :: SubFunctionIndex,
   -- | nestedFunctionCount :: Int
    --   A counter to keep track of the number of subfunctions created within a function.
    --   This information is used for generating unique names for subfunctions and for managing their nesting structure in the C code.
    nestedFunctionCount :: Int
  }

-- | The initial context state used at the beginning of code generation
startingState :: ContextState
startingState = ContextState 0 0

-- | Newtype representing a monadic computation with access to a `CompilationContext` and state for generating generateFreshName variables
newtype CompilerState a = CompilerState (ReaderT CompilationContext (State ContextState) a)
  deriving (Functor, Applicative, Monad, MonadReader CompilationContext, MonadState ContextState)

-- | This type combines the following monad transformers:
--
--   * `ReaderT CompilationContext`: Allows the computation to access and utilize the current `CompilationContext` information.
--   * `State ContextState`: Enables the computation to modify and query the internal `ContextState` for generating generateFreshName variables and tracking subfunctions.

-- | LambdaFunction to run a contextful computation
runContextMonad :: CompilerState a -> a
runContextMonad (CompilerState m) =
  -- This unpacks the nested monad transformers and executes the computation.
  -- 1. `runReaderT startingContext m`:
  --    * Runs the computation `m` with the initial `startingContext`.
  --    * The computation can access and use information from the context (storages, locations).
  -- 2. `runState startingState`:
  --    * Runs the computation with the initial `startingState` (generateFreshName index, subfunction count).
  --    * The computation can modify the state (increment generateFreshName index, update subfunction count).
  -- 3. `fst`: Extracts the final result (`a`) from the monad transformer stack.
  m |> (`runReaderT` startingContext) |> (`runState` startingState) |> fst


-- | Generate a generateFreshName variable index within the `CompilerState` computation
generateFreshName :: CompilerState SubFunctionIndex
generateFreshName = do
  -- Get the current generateFreshName index value from the `ContextState`.
  current <- gets freshIndex
  -- Modify the `ContextState` by incrementing the generateFreshName index.
  modify' (\s -> s {freshIndex = current + 1})
  -- Return the current generateFreshName index value.
  return current

-- | Retrieve the storage associated with a variable name within the `CompilerState` computation
getStorage :: ValueIdentifier -> CompilerState VariableStorage
getStorage name = asks (storages >>> Map.findWithDefault err name)
  where
    err = error ("No storage found for: " <> show name)

-- | Retrieve the location associated with a variable name within the `CompilerState` computation
getLocation :: ValueIdentifier -> CompilerState Location
getLocation name = asks (locations >>> Map.findWithDefault err name)
  where
    err = error ("No location found for: " <> show name)

-- | Execute a `CompilerState` computation with additional variable storages in scope
withStorages :: [(ValueIdentifier, VariableStorage)] -> CompilerState a -> CompilerState a
withStorages newStorages computation =
  -- Local function to modify the context's `storages` map.
  local (\r -> r {storages = Map.fromList newStorages <> storages r}) $
    -- Run the computation `computation` with the modified context.
    computation

-- | Execute a `CompilerState` computation with additional variable locations in scope
applyLocations :: [(ValueIdentifier, Location)] -> CompilerState a -> CompilerState a
applyLocations newLocations computation =
  -- Local function to modify the context's `locations` map.
  local (\r -> r {locations = Map.fromList newLocations <> locations r}) $
    -- Run the computation `computation` with the modified context.
    computation

-- | Increment the number of subfunctions created within the current context
addNSubFunctions :: Int -> CompilerState ()
addNSubFunctions more =
  -- Modify the `ContextState` by adding `more` to the `nestedFunctionCount` counter.
  modify' (\s -> s {nestedFunctionCount = nestedFunctionCount s + more})

-- | Execute a `CompilerState` computation with a reset subfunction creation counter
resetSubfunctionCreationCounter :: CompilerState a -> CompilerState a
resetSubfunctionCreationCounter m = do
  -- Get the current subfunction creation count from the `ContextState`.
  old <- gets nestedFunctionCount
  -- Reset the `nestedFunctionCount` counter in the `ContextState` to 0.
  modify' (\s -> s {nestedFunctionCount = 0})
  -- Run the computation `m` with the reset counter.
  ret <- m
  -- Restore the previous subfunction creation count after the computation.
  modify' (\s -> s {nestedFunctionCount = old})
  -- Return the result of the computation.
  return ret


-- | Convert an atom to an `Int` location within the `CompilerState` computation, ensuring type compatibility.
atomToInteger :: Atom -> CompilerState Location
atomToInteger atom =
  case atom of
    -- PrimitiveValue integer atom: Return the corresponding `PrimIntLocation`.
    PrimitiveAtom (PrimInt num) -> return (PrimIntLocation num)
    -- Variable name atom:
    NameAtom name -> do
      -- Get the location associated with the variable name.
      loc <- getLocation name
      -- Check if the location type is `IntegerVariable`.
      case getLocationType loc of
        IntegerVariable -> return loc
        -- If not `IntegerVariable`, raise an error indicating type mismatch.
        _ -> error ("'" <> name <> "' has location " <> show loc <> " which cannot hold an Int")
    -- Any other atom type: Raise an error indicating invalid conversion.
    _ -> error ("Cannot use atom " <> show atom <> " as an Int")

-- | Convert an atom to a `String` location within the `CompilerState` computation, ensuring type compatibility.
atomToString :: Atom -> CompilerState Location
atomToString atom =
  case atom of
    -- PrimitiveValue string atom: Return the corresponding `PrimStringLocation`.
    PrimitiveAtom (PrimString str) -> return (PrimStringLocation str)
    -- Variable name atom:
    NameAtom name -> do
      -- Get the location associated with the variable name.
      loc <- getLocation name
      -- Check if the location type is `StringVariable`.
      case getLocationType loc of
        StringVariable -> return loc
        -- If not `StringVariable`, raise an error indicating type mismatch.
        _ -> error ("'" <> name <> "' has location " <> show loc <> " which cannot hold a String")
    -- Any other atom type: Raise an error indicating invalid conversion.
    _ -> error ("Cannot use atom " <> show atom <> " as a String")

-- | Convert an atom to a pointer location within the `CompilerState` computation, ensuring type compatibility.
atomAsPointer :: Atom -> CompilerState Location
atomAsPointer atom =
  case atom of
    -- Variable name atom:
    NameAtom name -> do
      -- Get the location associated with the variable name.
      loc <- getLocation name
      -- Check if the location type is `PointerVariable`.
      case getLocationType loc of
        PointerVariable -> return loc
        -- If not `PointerVariable`, raise an error indicating type mismatch.
        _ -> error ("'" <> name <> "' has location " <> show loc <> " which cannot hold a pointer")
    -- Any other atom type: Raise an error indicating invalid conversion.
    _ -> error ("Cannot use atom " <> show atom <> " as a pointer")


-- | Generate Cmm instructions for built-in operations within the `CompilerState` monad.
generateBuiltinInstructions :: BuiltinFunction -> [Atom] -> CompilerState [IRInstruction]
generateBuiltinInstructions builtin arguments =
  case builtin of
    -- Arithmetic operations
    Add -> do
      -- Convert the first two arguments to integer locations.
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      -- Generate BuiltinFunction2 instruction for addition and an EnterCaseContinuation.
      return [BuiltinFunction2 Add2 l1 l2, EnterCaseContinuation]
    Sub -> do
      -- Convert the first two arguments to integer locations.
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      -- Generate BuiltinFunction2 instruction for addition and an EnterCaseContinuation.
      return [BuiltinFunction2 Add2 l1 l2, EnterCaseContinuation]
    Mul -> do
      -- Convert the first two arguments to integer locations.
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      -- Generate BuiltinFunction2 instruction for addition and an EnterCaseContinuation.
      return [BuiltinFunction2 Add2 l1 l2, EnterCaseContinuation]
    Div -> do
      -- Convert the first two arguments to integer locations.
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      -- Generate BuiltinFunction2 instruction for addition and an EnterCaseContinuation.
      return [BuiltinFunction2 Add2 l1 l2, EnterCaseContinuation]
      -- Similar logic for subtraction, multiplication, and division builtins.
    -- Comparison operations
    Less -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 Less2 l1 l2, EnterCaseContinuation]
    LessEqual -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 LessEqual2 l1 l2, EnterCaseContinuation]
    Greater -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 Greater2 l1 l2, EnterCaseContinuation]
    GreaterEqual -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 GreaterEqual2 l1 l2, EnterCaseContinuation]
    EqualTo -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 EqualTo2 l1 l2, EnterCaseContinuation]
    NotEqualTo -> do
      (l1, l2) <- getTwoArgumentLocations atomToInteger arguments
      return [BuiltinFunction2 NotEqualTo2 l1 l2, EnterCaseContinuation]

    Concat -> do
      -- Convert the first two arguments to string locations.
      (l1, l2) <- getTwoArgumentLocations atomToString arguments
      -- Generate BuiltinFunction2 instruction for string concatenation and EnterCaseContinuation.
      return [BuiltinFunction2 Concat2 l1 l2, EnterCaseContinuation]
    Negate -> do
      -- Convert the first argument to an integer location.
      l <- getOneArgumentLocation atomToInteger arguments
      -- Generate BuiltinFunction1 instruction for negation and EnterCaseContinuation.
      return [BuiltinFunction1 Negate1 l, EnterCaseContinuation]
    ExitWithInt -> do
      -- Convert the first argument to an integer location.
      l <- getOneArgumentLocation atomToInteger arguments
      -- Generate BuiltinFunction1 instruction for printing an integer and Exit instruction.
      return [BuiltinFunction1 PrintInt1 l, Exit]
    ExitWithString -> do
      -- Convert the first argument to a string location.
      l <- getOneArgumentLocation atomToString arguments
      -- Generate BuiltinFunction1 instruction for printing a string and Exit instruction.
      return [BuiltinFunction1 PrintString1 l, Exit]
  where
    -- Helper function to get two argument locations for binary builtins
    getTwoArgumentLocations :: (Atom -> CompilerState Location) -> [Atom] -> CompilerState (Location, Location)
    getTwoArgumentLocations convert atoms =
      forM atoms convert >>= \case
        [l1, l2] -> return (l1, l2)
        _ -> error ("Expected 2 locations for builtin " ++ show builtin ++ ", found " ++ show (length atoms))

    -- Helper function to get one argument location for unary builtins
    getOneArgumentLocation :: (Atom -> CompilerState Location) -> [Atom] -> CompilerState Location
    getOneArgumentLocation convert atoms =
      forM atoms convert >>= \case
        [l] -> return l
        _ -> error ("Expected 1 location for builtin " <> show builtin ++ ", found " <> show (length atoms))

-- | Generate the function body for a case expression within the `CompilerState` monad.
generateCaseFunction :: Int -> [ValueIdentifier] -> CaseBranches -> CompilerState LambdaFunction
generateCaseFunction index bound alts = do
  -- Reset the subfunction creation counter to avoid conflicts within the case function.
  resetSubfunctionCreationCounter <| do
    -- Extract argument information (number of pointers, integers, strings) from bound variables.
    (boundArguments, buriedLocations) <- getBuriedArgs
    -- Introduce the bound variables into the current scope with their corresponding locations (Buried* types).
    applyLocations buriedLocations <| do
      -- Generate the function body and any nested subfunctions for the case branches.
      (body, subFunctions) <- handleAlts alts
      -- Return the constructed LambdaFunction definition.
      return LambdaFunction {..}

    where
        -- LambdaFunction name and properties
        functionName = LambdaCaseFunction index  -- Use the case expression index for naming.
        closureType = DynamicClosure      -- Case functions are dynamically closed.
        argumentsCount = 0                        -- Case functions don't take arguments.

        -- Helper function to extract argument information and locations for bound variables
        getBuriedArgs :: CompilerState (ArgumentInfo, [(ValueIdentifier, Location)])
        getBuriedArgs = do
          (ptrs, ints, strings) <- separateBoundVariablesByType bound
          let argInfo = ArgumentInfo (length ptrs) (length ints) (length strings)
              locations =
                    zip ptrs (map Buried [0 ..])  -- Use Buried for pointers
                      <> zip ints (map BuriedInt [0 ..]) -- Use BuriedInt for integers
                      <> zip strings (map BuriedString [0 ..]) -- Use BuriedString for strings
          return (argInfo, locations)
        
        -- | Generate the function body and any nested subfunctions for case expression branches within the `CompilerState` monad.
        handleAlts :: CaseBranches -> CompilerState (FunctionBody, [LambdaFunction])
        handleAlts = \case
        -- LambdaBinding a primitive integer or string value to a variable
          Unbox StringBox name expr ->
            withStorageAndLocation name StringVariable StringRegister (createDirectBodyUpdate StringUpdate expr)
          IntegerCases branches defaultExpr ->
            handleCaseBranches IntCaseBody (const constructFunctionBody) branches defaultExpr
          StringCases branches defaultExpr ->
            handleCaseBranches StringCaseBody (const constructFunctionBody) branches defaultExpr
          BindPrim IntBox name expr ->
            withStorageAndLocation name IntegerVariable IntRegister (createDirectBodyUpdate IntUpdate expr)
          BindPrim StringBox name expr ->
            withStorageAndLocation name StringVariable StringRegister (createDirectBodyUpdate StringUpdate expr)
          Unbox IntBox name expr ->
            withStorageAndLocation name IntegerVariable IntRegister (createDirectBodyUpdate IntUpdate expr)
          ConstructorAlternatives branches defaultExpr ->
            handleCaseBranches makeCaseBody generateConstructorCaseBody branches defaultExpr
            where
              makeCaseBody :: [(( ConstructorTag, [ValueIdentifier]), CodeBlock)] -> CodeBlock -> FunctionBody
              makeCaseBody branches' defaultBody =
                let withoutNames = [(tag, body) | ((tag, _), body) <- branches']
                    removeExcessArgs = CodeBlock mempty 0 [PopExcessConstructorArgs]
                in TagCaseBody withoutNames (removeExcessArgs <> defaultBody)

              generateConstructorCaseBody :: ( ConstructorTag, [ValueIdentifier]) -> SimplifiedExpression -> CompilerState (CodeBlock, [LambdaFunction])
              generateConstructorCaseBody (_, names) expr =
                let storages = zip names (repeat (LocalStorage PointerVariable))
                    locations = zip names (map ConstructorArgument [0 ..])
                in withStorages storages <| applyLocations locations <| do
                      (body, fns) <- constructFunctionBody expr
                      -- We include the number of names as the number of constructor arguments
                      return (CodeBlock mempty (length names) mempty <> body, fns)
          where
            withStorageAndLocation name typ location =
              withStorages [(name, LocalStorage typ)] >>> applyLocations [(name, location)]

            createDirectBodyUpdate :: UpdateType -> SimplifiedExpression -> CompilerState (FunctionBody, [LambdaFunction])
            createDirectBodyUpdate updateType expr = do
              (body, subFunctions) <- constructFunctionBody expr
              return (ContinuationBody updateType body, subFunctions)

          -- -- Unboxing an integer or string from a boxed value (similar to BindPrim)
          -- Unbox (tbox :: IntBox | StringBox) name expr ->
          --     let (varType, varLoc) = case tbox of
          --                         IntBox -> (IntegerVariable, IntRegister)
          --                         StringBox -> (StringVariable, StringRegister)
          --     withStorageAndLocation name varType varLoc (createDirectBodyUpdate (case tbox of
          --                                                         IntBox -> IntUpdate
          --                                                         StringBox -> StringUpdate) expr)

          -- -- Handling branches for integer case expressions
          -- IntegerCases branches defaultExpr ->
          --     handleCaseBranches IntCaseBody (const constructFunctionBody) branches defaultExpr

          -- -- Handling branches for string case expressions (similar to IntegerCases)
          -- StringCases branches defaultExpr ->
          --     handleCaseBranches StringCaseBody (const constructFunctionBody) branches defaultExpr

          -- -- Handling branches for constructor case expressions
          -- ConstructorAlternatives branches defaultExpr ->
          --     handleCaseBranches makeCaseBody generateConstructorCaseBody branches defaultExpr
          --     where
          --         -- Helper function to create a case body for constructor case expressions
          --         makeCaseBody :: [(( ConstructorTag, [ValueIdentifier]), CodeBlock)] -> CodeBlock -> FunctionBody
          --         makeCaseBody branches' defaultBody =
          --         let withoutNames = [(tag, body) | ((tag, _), body) <- branches']   -- Remove variable names from branches
          --             removeExcessArgs = CodeBlock mempty 0 [PopExcessConstructorArgs]   -- IRInstruction to pop excess constructor arguments
          --         in TagCaseBody withoutNames (removeExcessArgs <> defaultBody)

                  -- -- Helper function to generate code for a constructor case branch
                  -- generateConstructorCaseBody :: ( ConstructorTag, [ValueIdentifier]) -> SimplifiedExpression -> CompilerState (CodeBlock, [LambdaFunction])
                  -- generateConstructorCaseBody (tag, names) expr = do
                  -- let storages = zip names (repeat (LocalStorage PointerVariable))  -- Allocate storage for each variable
                  --     locations = zip names (map ConstructorArgument [0 ..])        -- Assign constructor argument locations
                  -- withStorages storages <| applyLocations locations <| do
                  --     (body, fns) <- constructFunctionBody expr                           -- Generate code for the expression
                  --     -- Return body with the number of names as the constructor argument count
                  --     return (CodeBlock mempty (length names) mempty <> body, fns)
              -- Helper function to combine storage and location assignments
              -- withStorageAndLocation name typ location =
              --     withStorages [(name, LocalStorage typ)] >>> applyLocations [(name, location)]
                  -- Use monad sequencing (>>>) to perform storage and location assignments sequentially.

    --           -- Helper function to create a function body for direct variable updates
    --           createDirectBodyUpdate :: UpdateType -> SimplifiedExpression -> CompilerState (FunctionBody, [LambdaFunction])
    --           createDirectBodyUpdate updateType expr = do
    --               (body, subFunctions) <- constructFunctionBody expr  -- Generate code for the expression
    --               return (ContinuationBody updateType body, subFunctions)  -- Wrap the body with a ContinuationBody
    -- -- | Generate the function body and any nested subfunctions for branches within the `CompilerState` monad.
        handleCaseBranches ::
          ([(a, CodeBlock)] -> CodeBlock -> FunctionBody) ->
          (a -> SimplifiedExpression -> CompilerState (CodeBlock, [LambdaFunction])) ->
          [(a, SimplifiedExpression)] ->
          Maybe SimplifiedExpression ->
          CompilerState (FunctionBody, [LambdaFunction])
        handleCaseBranches makeBody genBody branches defaultExpr = do
        -- Process each branch: generate code and collect subfunctions
          branches' <- forM branches <| \(i, branch) -> do
              (body, subFunctions) <- genBody i branch
              return ((i, body), subFunctions)

          -- Generate code for the default expression (if present) and collect subfunctions
          default' <- forM defaultExpr constructFunctionBody

          -- Destructure processed branches and default expression
          let branchBodies = map fst branches'  -- Extract branch bodies
              branchSubFunctions = foldMap snd branches'  -- Combine subfunctions from all branches
              (defaultBody, defaultSubFunctions) = fromMaybe (mempty, []) default'  -- Extract default body and subfunctions (or use empty defaults)
              -- Construct the final case body using the makeBody function
              body = makeBody branchBodies defaultBody
              -- Combine subfunctions from branches and the default expression
              subFunctions = branchSubFunctions <> defaultSubFunctions

          return (body, subFunctions)



-- | Generate the function body and subfunctions for a case expression within the `CompilerState` monad.
genCaseExpr :: SimplifiedExpression -> [ValueIdentifier] -> CaseBranches -> CompilerState (CodeBlock, [LambdaFunction])
genCaseExpr scrut bound alts = do
  -- Get the current subfunction creation counter.
  index <- gets nestedFunctionCount

  -- Generate the case function definition.
  caseFunction <- generateCaseFunction index bound alts

  -- Increment the subfunction creation counter.
  addNSubFunctions 1

  -- Generate the code for the scrutinee expression (the value being matched in the case).
  (scrutBody, scrutFunctions) <- constructFunctionBody scrut

  -- Get instructions to createBuryInstruction bound variables (make them inaccessible outside the case expression).
  buryBound <- getInstructionsToBuryBound bound

  -- Construct the main body of the case expression
  let thisBody = CodeBlock mempty 0 (buryBound <> [PushCaseContinuation index])

  -- Combine the main body, scrutinee body, case function, and subfunctions from scrutinee
  return (thisBody <> scrutBody, caseFunction : scrutFunctions)

  where
    -- Helper function to generate instructions for burying bound variables
    getInstructionsToBuryBound :: [ValueIdentifier] -> CompilerState [IRInstruction]
    getInstructionsToBuryBound bound = do
      (ptrs, ints, strings) <- separateBoundVariablesByType bound   -- Separate bound variables by type
      -- Generate burying instructions in reverse order to preserve stack order
      buryPtrs <- foldMapM createBuryInstruction (reverse ptrs)
      buryInts <- foldMapM createBuryInstruction (reverse ints)
      buryStrings <- foldMapM createBuryInstruction (reverse strings)
      return (buryStrings <> buryInts <> buryPtrs)

    -- Helper function to generate a burying instruction based on variable type
    createBuryInstruction :: ValueIdentifier -> CompilerState [IRInstruction]
    createBuryInstruction name = do
      loc <- getLocation name           -- Get the location of the bound variable
      return <| case getLocationType loc of -- Generate burying instruction based on location type
                 PointerVariable -> [Bury loc]
                 IntegerVariable -> [BuryInt loc]
                 StringVariable -> [BuryString loc]



-- | Generate the function body and subfunctions for a let expression within the `CompilerState` monad.
genLet :: [LambdaBinding] -> SimplifiedExpression -> CompilerState (CodeBlock, [LambdaFunction])
genLet bindings expr = do
    -- Get storage information for the let bindings.
    bindingStorages <- getStorageInformationForBindings

    -- Allocate storage for all let bindings within the current scope.
    withStorages bindingStorages <| do
        let tableCount = bindingStorages   -- Extract storage information
                        |> filter (snd >>> (== LocalStorage PointerVariable)) -- Filter pointer variables
                        |> length                                       -- Count pointer variables
        allocations <- allocateStorageForTables tableCount  -- Allocate storage for tables

        -- Get locations for all variables (bound and used in the expression).
        locations <- getAllVariableLocations

        -- Allocate locations for variables and generate subfunctions within the scope.
        applyLocations locations <| do
          subFunctions <- generateSublambdaFunctions   -- Generate subfunctions
          letInstrs <- generateLetInstructions         -- Generate let instructions

          -- Construct the main body of the let expression
          let thisBody = CodeBlock allocations 0 letInstrs

          -- Update subfunction counter after generating let instructions (they might use subfunctions)
          addNSubFunctions (length subFunctions)

          -- Generate code for the expression within the let scope
          (exprBody, exprSubFunctions) <- constructFunctionBody expr

          -- Combine the main body, expression body, subfunctions, and expression subfunctions
          return (thisBody <> exprBody, subFunctions <> exprSubFunctions)

    where
        -- Helper function to get storage information for each binding
        getStorageInformationForBindings :: CompilerState [(ValueIdentifier, VariableStorage)]
        getStorageInformationForBindings =
          forM bindings <| \(LambdaBinding name form) -> do
              storage <- case form of
                LambdaExpression [] N _ _ -> GlobalStorage <$> generateFreshName  -- Allocate global storage for anonymous functions
                _ -> return (LocalStorage PointerVariable)          -- LocalStorage with PointerVariable for other bindings
              return (name, storage)

        -- -- Helper function to allocate storage for tables (if needed)
        -- allocateStorageForTables :: Int -> CompilerState [IRInstruction]
        -- allocateStorageForTables tableCount = replicateM tableCount NewTable

        -- Helper function to allocate storage for tables (if needed)
        allocateStorageForTables :: Int -> CompilerState MemoryAllocation
        allocateStorageForTables tableCount = do
          formAllocations <- foldMapM (\(LambdaBinding _ form) -> allocateStorageForLambdaForm form) bindings
          return (MemoryAllocation tableCount 0 0 0 <> formAllocations)
          where
              -- Helper function to allocate storage based on lambda form
              allocateStorageForLambdaForm :: LambdaExpression -> CompilerState MemoryAllocation
              allocateStorageForLambdaForm (LambdaExpression [] _ _ _) =
              -- Allocate one unit of storage for the blank pointer (if no bound arguments)
              -- This becomes more complex for integer arguments not at least pointer-sized.
                return (MemoryAllocation 0 1 0 0)
              allocateStorageForLambdaForm (LambdaExpression bound _ _ _) = do
                (boundPtrs, boundInts, boundStrings) <- separateBoundVariablesByType bound
                -- Allocate storage for bound variables based on their types
                return (MemoryAllocation 0 (length boundPtrs) (length boundInts) (length boundStrings))
      
        -- | Get locations for all variables (bound and used in the expression) within the `CompilerState` monad.
        getAllVariableLocations :: CompilerState [(ValueIdentifier, Location)]
        getAllVariableLocations = do
          start <- gets nestedFunctionCount
          forM (zip [start ..] bindings) <| \(i, LambdaBinding name _) -> do
            storage <- getStorage name
            case storage of
              GlobalStorage index -> return (name, Global index)
              CAFStorage index -> return (name, CAF index)
              LocalStorage PointerVariable -> return (name, Allocated i)
              other -> error (show other <> " is not a valid storage for the closure " <> show name)

        -- | Generate subfunctions (functions defined within let expressions) within the `CompilerState` monad.
        generateSublambdaFunctions :: CompilerState [LambdaFunction]
        generateSublambdaFunctions = forM bindings generateLambdaBindingFunction

        -- Helper function to generate instructions for let bindings
        generateLetInstructions :: CompilerState [IRInstruction]
        generateLetInstructions = do
        -- Get the current subfunction creation counter.
          start <- gets nestedFunctionCount
          -- Allocate storage for each binding using foldMapM and allocateBinding.
          foldMapM (uncurry allocateBinding) (zip [start ..] bindings)
          where
              -- Helper function to allocate storage for a single binding
              allocateBinding :: Int -> LambdaBinding -> CompilerState [IRInstruction]
              allocateBinding i (LambdaBinding name (LambdaExpression bound _ _ _)) = do
              -- Get the storage type for the variable.
                storage <- getStorage name
                -- Allocate instructions only for non-global storage.
                case storage of
                  GlobalStorage _ -> return []
                  _ -> do
                  -- Get locations for all bound variables within the lambda form.
                    locations <- forM bound getLocation
                    -- Helper function to filter locations and generate allocation instructions based on type
                    let alloc typ mk = locations |> filter (getLocationType >>> (== typ)) |> map mk
                        -- Allocate instructions for pointers, integers, and strings
                        allocPtrs = alloc PointerVariable AllocPointer
                        allocInts = alloc IntegerVariable AllocInt
                        allocStrings = alloc StringVariable AllocString

                        -- Allocate a blank pointer if there are no bound arguments (for closure garbage collection)
                        -- This assumes integers are at least as big as pointers (common case).
                        allocBlank = [AllocBlankPointer | null bound]

                    -- Combine all allocation instructions
                    return ([AllocTable i] <> allocBlank <> allocPtrs <> allocInts <> allocStrings)
    
-- | Generate the function body and subfunctions for an expression within the `CompilerState` monad.
constructFunctionBody :: SimplifiedExpression -> CompilerState (CodeBlock, [LambdaFunction])
constructFunctionBody = \case
  -- Let expression
  Let bindings expr -> genLet bindings expr  -- Delegate to genLet for handling let expressions

  -- Case expression
  Case scrut bound alts -> genCaseExpr scrut bound alts -- Delegate to genCaseExpr for handling case expressions

  -- Error expression
  Error err ->
      return (justInstructions [PrintError err, Exit])  -- Generate instructions for printing error message and exiting

  -- PrimitiveValue integer literal
  PrimitiveValue (PrimInt i) ->
      return (justInstructions [StoreInt (PrimIntLocation i), EnterCaseContinuation]) -- Store integer and enter case continuation

  -- PrimitiveValue string literal
  PrimitiveValue (PrimString s) ->
      let instrs = [StoreString (PrimStringLocation s), EnterCaseContinuation]
      in return (CodeBlock (MemoryAllocation 0 0 0 0) 0 instrs, []) -- Store string, create body, and return no subfunctions

  -- Boxed integer expression
  Box IntBox atom -> do
      loc <- atomToInteger atom  -- Get location of the integer value
      return (justInstructions [StoreInt loc, EnterCaseContinuation]) -- Store integer and enter case continuation

  -- Boxed string expression
  Box StringBox atom -> do
      loc <- atomToString atom  -- Get location of the string value
      return (justInstructions [StoreString loc, EnterCaseContinuation]) -- Store string and enter case continuation

  -- Function application
  Apply f arguments -> do
      fLoc <- getLocation f  -- Get location of the function
      argLocs <- mapM atomAsPointer arguments -- Get locations of the arguments
      let instrs = map PushSA (reverse argLocs) <> [Enter fLoc] -- Push arguments and enter function
      return (justInstructions instrs) -- Return body with instructions and no subfunctions

  -- Constructor expression
  Constructor tag arguments -> do
      argLocs <- mapM atomAsPointer arguments  -- Get locations of the arguments
      let instrs =
            [StoreTag tag, StoreConstructorArgCount (length arguments)]  -- Store constructor tag and argument count
                <> map PushConstructorArg (reverse argLocs)  -- Push arguments for constructor
                <> [EnterCaseContinuation]
      return (justInstructions instrs) -- Return body with instructions and no subfunctions

  -- Built-in function call
  BuiltinFunction b arguments -> do
      instrs <- generateBuiltinInstructions b arguments -- Generate instructions for the built-in function
      return (justInstructions instrs) -- Return body with instructions and no subfunctions

  where
      -- Helper function to create a CodeBlock record with just instructions and no allocations
      justInstructions instructions = (CodeBlock mempty 0 instructions, [])


-- | Separate bound variable names by their types within the `CompilerState` monad.
separateBoundVariablesByType :: [ValueIdentifier] -> CompilerState ([ValueIdentifier], [ValueIdentifier], [ValueIdentifier])
separateBoundVariablesByType bound = do
  -- Extract pointer variables
  ptrs <- extract PointerVariable
  -- Extract integer variables
  ints <- extract IntegerVariable
  -- Extract string variables
  strings <- extract StringVariable
  -- Return the separated lists of variable names
  return (ptrs, ints, strings)

  where
    -- Helper function to extract variables of a specific type
    extract :: VariableType -> CompilerState [ValueIdentifier]
    extract storageType =
      -- Filter the bound variable names based on their storage type
      filterM (getStorage >>> fmap (== LocalStorage storageType)) bound


-- | Generate a Cmm function from a lambda form within the `CompilerState` monad.
generateLambdaFunction :: FunctionName -> ClosureType -> LambdaExpression -> CompilerState LambdaFunction
generateLambdaFunction functionName closureType (LambdaExpression bound u arguments expr) =
  -- Allocate storage for function arguments (all pointers by default)
  withStorages argStorages <|
  -- Get a generateFreshName subfunction creation counter
  resetSubfunctionCreationCounter <| do
    let argumentsCount = length arguments  -- Number of arguments
    (boundPtrs, boundInts, boundStrings) <- separateBoundVariablesByType bound  -- Separate bound variables by type
    let boundArguments = ArgumentInfo (length boundPtrs) (length boundInts) (length boundStrings) -- Argument information

    -- Get location information for the function itself (if applicable)
    myLocation <- getFunctionLocationInformation functionName

    -- Combine locations for various parts:
    -- * Maybe function location (if a named function)
    -- * Bound variables (pointers, integers, strings)
    -- * Function arguments
    let locations =
          maybeToList myLocation
            <> boundLocations Bound boundPtrs
            <> boundLocations BoundInt boundInts
            <> boundLocations BoundString boundStrings
            <> argLocations

    -- Generate code for the function body and any subfunctions within the lambda form
    (normalBody, subFunctions) <- applyLocations locations (constructFunctionBody expr)

    -- Update instruction for handling closures based on the closure type and update type
    let updateExtra = case (closureType, u) of
          (GlobalClosure _, _) -> mempty
          (CAFClosure i, _) -> CodeBlock (MemoryAllocation 1 1 0 0) 0 [CreateCAFClosure i, PushUpdate]
          (DynamicClosure, N) -> mempty
          (DynamicClosure, U) -> CodeBlock mempty 0 [PushUpdate]
    -- Combine normal body with the update instruction (if needed)
    let body = NormalBody (updateExtra <> normalBody)

    -- Return the constructed LambdaFunction record
    return LambdaFunction {..}
  where
    -- Get location information for the function itself (if applicable)
    getFunctionLocationInformation :: FunctionName -> CompilerState (Maybe (ValueIdentifier, Location))
    getFunctionLocationInformation = \case
      PlainFunction name -> do
        storage <- getStorage name  -- Get storage type for the function name
        return <| Just <| case storage of  -- Assign location based on storage type
          GlobalStorage index -> (name, Global index)
          CAFStorage index -> (name, CAF index)
          LocalStorage PointerVariable -> (name, CurrentNode)  -- Current function (for recursive calls)
          s -> error ("VariableStorage " ++ show s ++ " is not a valid storage for a function")
      _ -> return Nothing  -- Anonymous functions don't have their own location

    -- Default storage and location information for function arguments
    argStorages :: [(ValueIdentifier, VariableStorage)]
    argStorages = zip arguments (repeat (LocalStorage PointerVariable))

    argLocations :: [(ValueIdentifier, Location)]
    argLocations = zip arguments (map Argument [0 ..])  -- Arguments are at offsets 0, 1, ...

    -- Generate location information for bound variables based on type and index
    boundLocations :: (Int -> Location) -> [ValueIdentifier] -> [(ValueIdentifier, Location)]
    boundLocations f names = zip names (map f [0 ..])


-- | Generate a Cmm function from a binding within the `CompilerState` monad.
generateLambdaBindingFunction :: LambdaBinding -> CompilerState LambdaFunction
generateLambdaBindingFunction (LambdaBinding name form) = do
  -- Get the storage type associated with the binding name
  storage <- getStorage name

  -- Determine the closure type based on the storage type:
  -- * Global functions: GlobalClosure
  -- * Captured argument functions (closures): CAFClosure
  -- * Other bindings (let bindings): DynamicClosure
  let closureType' = case storage of
        GlobalStorage index -> GlobalClosure index
        CAFStorage index -> CAFClosure index
        _ -> DynamicClosure

  -- Generate the Cmm function using generateLambdaFunction
  generateLambdaFunction (PlainFunction name) closureType' form


-- | Generate Cmm code from an STG (Spineless Tagless G-machine) in a contextful way,
-- using the `CompilerState` monad for managing the context (storage, locations, etc.).

genCmm :: STG -> CompilerState Cmm
genCmm (STG bindings entryForm) = do
  -- Generate a generateFreshName index for the entry function
  entryIndex <- generateFreshName

  -- Process bindings to generate information for top-level functions:
  -- * Function name
  -- * VariableStorage type (GlobalStorage or CAFStorage)
  -- * Location (Global or CAF)
  topLevel <-
    forM bindings <| \(LambdaBinding name (LambdaExpression _ u _ _)) -> do
      index <- generateFreshName
      case u of
        N -> return (name, GlobalStorage index, Global index)
        U -> return (name, CAFStorage index, CAF index)

  -- Extract storage and location information from top-level function data
  let topLevelStorages = map (\(name, storage, _) -> (name, storage)) topLevel
      topLevelLocations = map (\(name, _, location) -> (name, location)) topLevel

  -- Execute actions within a context with top-level storage and locations
  withStorages topLevelStorages <| applyLocations topLevelLocations <| do
    -- Generate the entry function
    entry <- generateLambdaFunction Entry (GlobalClosure entryIndex) entryForm
    -- Generate top-level functions from bindings
    topLevelFunctions <- forM bindings generateLambdaBindingFunction
    -- Return the Cmm structure with entry function and top-level functions
    return (Cmm topLevelFunctions entry)

-- | Generate Cmm code from an STG (Spineless Tagless G-machine). This is a convenience
-- function that combines `genCmm` with `runContextMonad` to lift the contextful
-- generation process into a pure function returning the final Cmm data structure.
cmm :: STG -> Cmm
cmm = genCmm >>> runContextMonad




