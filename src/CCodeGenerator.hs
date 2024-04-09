{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}



module CCodeGenerator (generateCCode) where

import Cmm hiding (cmm)

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (Foldable (fold))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Ourlude
import Text.Printf (printf)


-- | Type representing a mapping between unique integer identifiers and function paths.
--
-- This type, `GlobalFunctionMap`, is used to store information about global functions
-- within the generated C code. The key of the `IntMap` is an integer that uniquely identifies
-- a function, while the value is the function path. The function path originates
-- from the parsing or analysis stages of Tiny tinyGHC and helps identify the function within
-- the original tinyHaskell AST.
type GlobalFunctionMap = IntMap FunctionPath


{- Locations -}

-- | Type representing a map that associates locations with their corresponding C code.
--
-- The `CCodeLocationMap` type is a newtype wrapper around a `Map.Map Location CCode`. This
-- map stores information about how to access or use various locations (registers, variables)
-- in the generated C code. Each key is a `Location` value (e.g., `CurrentNode`, `IntRegister`),
-- and the corresponding value is the C code snippet that represents how to access or use
-- that location.
--
-- The comments within the code mention that this map is typically sparse, meaning it only
-- contains entries for locations that are actually used in the generated C code.
--
newtype CCodeLocationMap = CCodeLocationMap (Map.Map Location CCode)
  deriving (Show, Semigroup, Monoid)

-- | Function to retrieve the C code snippet for accessing a given location from the map.
--
-- The `findCCodeLocation` function takes a `CCodeLocationMap` and a `Location` as arguments.
-- It attempts to find the corresponding C code snippet for that location within the map.
-- If the location is found, the function returns a `Just` value containing the C code snippet.
-- Otherwise, it returns `Nothing`.
findCCodeLocation :: CCodeLocationMap -> Location -> Maybe CCode
findCCodeLocation (CCodeLocationMap mp) = \case
  CurrentNode -> Just "g_NodeRegister"
  IntRegister -> Just "g_IntRegister"
  StringRegister -> Just "g_StringRegister"
  -- Integers don't need to be allocation, so we can always use them directly
  -- Strings, on the other hand, do need explicit allocation, so they need an entry here
  PrimIntLocation i -> Just (show i)
  other -> Map.lookup other mp


-- | Function to create a `CCodeLocationMap` with a single entry for a location and its C code.
--
-- The `createSingleLocation` function takes a `Location` and a `CCode` string as arguments.
-- It creates a new `CCodeLocationMap` with a single key-value pair. The key is the provided
-- `Location`, and the value is the provided `CCode` string. This function is used
-- during code generation to associate specific locations with their corresponding C code snippets.
--
createSingleLocation :: Location -> CCode -> CCodeLocationMap
createSingleLocation loc code = CCodeLocationMap (Map.singleton loc code)


-- | Function to construct a `CCodeLocationMap` from a list of location-C code pairs.
--
-- This function, `manyLocations`, takes a list of tuples as input. Each tuple in the list
-- consists of two elements:
--   - The first element is a `Location` value, which specifies a location (e.g., register,
--     variable) within the generated C code.
--   - The second element is a `CCode` string, which represents the C code snippet
--     used to access or utilize that location.
--
-- The `manyLocations` function processes this list and constructs a new `CCodeLocationMap`.
-- This map associates each location with its corresponding C code snippet. The function
-- utilizes the `Map.fromList` function to create the map from the provided list of tuples.
-- Finally, it wraps the resulting map in the `CCodeLocationMap` newtype constructor.
--
-- This function is  used during the C code generation process to build a map that
-- keeps track of how to access or use various locations in the generated C code.
manyLocations :: [(Location, CCode)] -> CCodeLocationMap
manyLocations = Map.fromList >>> CCodeLocationMap

-- | Type representing C code.
--
-- The `CCode` type is a simple alias for `String`. This means that the generated C code
-- is currently represented as plain text strings. In a more efficient implementation,
-- a more structured type specifically designed for code representation might be used.
-- However, for explanatory purposes, a string is sufficient.
--
type CCode = String

{- Common variable names -}

-- | Function to generate a C variable name for a CAF cell (Closure Activation Frame cell).
--
-- The `generateCafCellIdentifier` function takes a `FunctionPath` as input. It  uses
-- the information in the path (potentially function name and arguments) to construct a unique
-- C variable name that identifies a cell within a Closure Activation Frame (CAF). This cell
-- would be used to store the function's environment or captured variables.
--
generateCafCellIdentifier :: FunctionPath -> CCode
generateCafCellIdentifier path = "caf_cell_for_" <> formatFunctionPath path

-- | A variable name used for calculating allocation size.
--
-- The `calculateAllocationSize` variable holds a constant string value, `"allocation_size"`.
-- This string is  used as a variable name within the generated C code. The purpose of this
-- variable is to store or reference the calculated size of some allocation during the code
-- generation process. This size might be used for memory allocation or other purposes related
-- to managing data structures in the generated C code.
calculateAllocationSize = "allocation_size"

-- | Function to generate a variable name for the Nth argument passed to a function.
--
-- The `generateArgVariable` function takes a `SubFunctionIndex` as an argument. This type 
-- represents an index or identifier for a sub-function (possibly a nested function within another
-- function). The function generates a C code identifier string that is prefixed with `"arg_"`
-- followed by the string representation of the index. This naming convention helps identify variables
-- that correspond to arguments passed to functions in the generated C code.
--
generateArgVariable :: SubFunctionIndex -> CCode
generateArgVariable n = "arg_" <> show n

-- | Function to generate a variable name for the Nth constructor argument.
--
-- The `generateConstructorArgVariable` function takes a `SubFunctionIndex` as an argument, similar
-- to `generateArgVariable`. It generates a C code identifier string that is prefixed with
-- `"constructor_arg_"` followed by the string representation of the index. This naming convention
-- is  used to identify variables that correspond to arguments passed to constructor functions
-- in the generated C code.
generateConstructorArgVariable :: SubFunctionIndex -> CCode
generateConstructorArgVariable n = "constructor_arg_" <> show n

-- | Function to generate a variable name for the Nth bound pointer in a closure.
--
-- The `generateBoundPointerVariable` function takes a `SubFunctionIndex` as an argument. It
-- generates a C code identifier string that is prefixed with `"bound_pointer_"` followed by the
-- string representation of the index. This naming convention is  used to identify variables
-- that correspond to bound pointers within closures (functions with captured variables) in the
-- generated C code. Closures require special handling due to their captured environment.
--
generateBoundPointerVariable :: SubFunctionIndex -> CCode
generateBoundPointerVariable n = "bound_pointer_" <> show n

-- | Function to generate a variable name for the Nth bound integer in a closure.
--
-- The `generateBoundIntVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"bound_int_"` followed by the string
-- representation of the index. This naming convention is  used to identify variables that
-- correspond to bound integer values within closures (functions with captured variables) in the
-- generated C code. Closures require special handling due to their captured environment, which
-- might include integer values.
generateBoundIntVariable :: SubFunctionIndex -> CCode
generateBoundIntVariable n = "bound_int_" <> show n

-- | Function to generate a variable name for the Nth bound string in a closure.
--
-- The `generateBoundStringVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"bound_string_"` followed by the string
-- representation of the index. This naming convention is  used to identify variables that
-- correspond to bound strings within closures in the generated C code. Closures can capture strings
-- as part of their environment.
generateBoundStringVariable :: SubFunctionIndex -> CCode
generateBoundStringVariable n = "bound_string_" <> show n

-- | A constant string used as a temporary variable name for a closure pointer.
--
-- The `temporaryClosurePointer` variable holds a constant string value, `"tmp_closure_ptr"`. This
-- string is  used as a temporary variable name within the generated C code, possibly
-- to hold a pointer to a closure during some operation.
temporaryClosurePointer :: CCode
temporaryClosurePointer = "tmp_closure_ptr"

-- | Function to generate a variable name for the Nth allocated sub-function.
--
-- The `generateAllocatedVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"allocated_"` followed by the string representation
-- of the index. This naming convention is  used to identify variables that correspond to
-- allocated sub-functions within the generated C code. Sub-functions might be nested functions
-- within other functions, and some might require allocation during code generation.
generateAllocatedVariable :: SubFunctionIndex -> CCode
generateAllocatedVariable n = "allocated_" <> show n

-- | Function to generate a variable name for the Nth buried pointer.
--
-- The `generateBuriedPointerVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"buried_ptr_"` followed by the string representation
-- of the index. This naming convention is  used to identify variables that correspond to buried
-- pointers within the generated C code. The exact meaning of "buried" in this context depends on the
-- specific implementation of Tiny tinyGHC. It might refer to pointers that are hidden within some data
-- structure or that are not directly accessible from the current scope.
generateBuriedPointerVariable :: SubFunctionIndex -> CCode
generateBuriedPointerVariable n = "buried_ptr_" <> show n

-- | Function to generate a variable name for the Nth buried integer.
--
-- The `generateBuriedIntVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"buried_int_"` followed by the string representation
-- of the index. This naming convention is  used to identify variables that correspond to buried
-- integer values within the generated C code. Similar to `generateBuriedPointerVariable`, the exact
-- meaning of "buried" depends on the Tiny tinyGHC implementation.
generateBuriedIntVariable :: SubFunctionIndex -> CCode
generateBuriedIntVariable n = "buried_int_" <> show n

-- | Function to generate a variable name for the Nth buried string.
--
-- The `generateBuriedStringVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"buried_string_"` followed by the string representation
-- of the index. This naming convention is  used to identify variables that correspond to buried
-- string values within the generated C code. Similar to the previous functions, the exact meaning of
-- "buried" depends on the Tiny tinyGHC implementation.
generateBuriedStringVariable :: SubFunctionIndex -> CCode
generateBuriedStringVariable n = "buried_string_" <> show n

-- | Function to generate a variable name for the Nth string literal.
--
-- The `generateStringLiteralVariable` function takes a `SubFunctionIndex` as an argument. It generates
-- a C code identifier string that is prefixed with `"string_literal_"` followed by the string representation
-- of the index. This naming convention is  used to identify variables that correspond to string literals
-- appearing in the original tinyHaskell code. String literals need to be stored and referenced during C code
-- generation.
generateStringLiteralVariable :: SubFunctionIndex -> CCode
generateStringLiteralVariable n = "string_literal_" <> show n

-- | Function to generate a variable name for the evacuation function based on argument information.
--
-- The `generateEvacuationInfoVariable` function takes an `ArgumentInfo` record as an argument. This record
--  originates from the analysis stages of Tiny tinyGHC and contains information about the bound arguments
-- of a function. Bound arguments refer to variables that are captured by a closure (function with captured
-- environment) or that are part of a function's definition. The function generates a C code identifier string
-- that describes the evacuation information for the function. Evacuation refers to the process of saving the
-- closure's environment (bound values) before a function call. The generated variable name helps identify
-- the evacuation function associated with a specific set of bound arguments.
generateEvacuationInfoVariable :: ArgumentInfo -> CCode
generateEvacuationInfoVariable (ArgumentInfo 0 0 0) = "evac_empty"
generateEvacuationInfoVariable ArgumentInfo {..} =
  "evac" <> fold
      [ part "pointers" boundPointers,
        part "ints" boundInts,
        part "strings" boundStrings
      ]
  where
    -- | Helper function used within `generateEvacuationInfoVariable` to construct a part of the variable name.
    --
    -- The `part` function is a helper function used internally by `generateEvacuationInfoVariable`. It takes a string
    -- `tag` and an integer `n` as arguments. The `tag`  represents a type category (e.g., "pointers", "ints",
    -- "strings") and `n` indicates the number of elements of that type in the bound arguments. The function constructs
    -- a string that combines the `tag` with an underscore, the string representation of `n`, and another underscore.
    -- This string is then concatenated with other parts generated for different type categories to form the complete
    -- evacuation information variable name.
    --
    part _ 0 = ""
    part tag n = "_" <> show n <> "_" <> tag



{- Nested Identifiers -}

-- | Type representing a sequence of function names.
--
-- The `FunctionPath` newtype defines a type to represent a sequence of function names. This type is 
-- used as an intermediate data structure during code generation. Tiny tinyGHC translates tinyHaskell code with potentially
-- nested functions into C code. The `FunctionPath` type helps manage these nested function calls by keeping
-- track of the sequence of function names encountered while traversing the tinyHaskell AST (Abstract Syntax Tree).
-- The AST represents the structure of the tinyHaskell program. By storing the function names in a list, `FunctionPath`
-- provides a way to construct the full path (sequence) of nested function calls for code generation purposes.
newtype FunctionPath = FunctionPath [FunctionName] deriving (Show, Eq)

-- | Instance of the `Semigroup` class for `FunctionPath`.
--
-- The `Semigroup` class defines an operation for combining two values of a type. In the context of
-- `FunctionPath`, the `<>` operator (also known as append) is used to concatenate two `FunctionPath` values.
-- This operation combines the sequences of function names from both paths into a single new sequence.
-- This is useful when building the full path during traversal of the nested function calls in the AST.
--
-- **Example:**
--
-- ```haskell
-- path1 = FunctionPath ["f", "g"]
-- path2 = FunctionPath ["h"]
-- combinedPath = path1 <> path2
-- -- combinedPath will be FunctionPath ["f", "g", "h"]
-- ```
instance Semigroup FunctionPath where
  FunctionPath names <> FunctionPath names' = FunctionPath (names <> names')

-- | Instance of the `Monoid` class for `FunctionPath`.
--
-- The `Monoid` class is a subclass of `Semigroup` that additionally defines an empty element (identity element)
-- for the combination operation. In the context of `FunctionPath`, the empty element (`mempty`) represents an
-- empty sequence of function names. This is useful for initialization or as a starting point for building paths.
--
-- **Example:**
--
-- 
-- emptyPath = mempty :: FunctionPath
-- -- emptyPath will be FunctionPath []
instance Monoid FunctionPath where
  mempty = FunctionPath []


-- | Function to add a function name to the end of a function path.
--
-- The `appendFunctionName` function takes a `FunctionName` as the first argument and a `FunctionPath` as the
-- second argument. It returns a new `FunctionPath` that is the original path with the provided function name
-- appended to the end. This function is  used during code generation to build the full sequence of function
-- names for nested function calls encountered in the tinyHaskell AST.
--
-- **Example:**
--
-- ```haskell
-- path = FunctionPath ["f", "g"]
-- name = "h"
-- appendedPath = appendFunctionName name path
-- -- appendedPath will be FunctionPath ["f", "g", "h"]
-- ```
appendFunctionName :: FunctionName -> FunctionPath -> FunctionPath
appendFunctionName name = (FunctionPath [name] <>)

-- | Function to format a function path as a valid C code string.
--
-- The `formatFunctionPath` function takes a `FunctionPath` as an argument and returns a string representation
-- of the path that is suitable for use in generated C code. This function performs several transformations on
-- the sequence of function names:
--
-- 1. **Reverse:** The order of names is reversed to match the C convention of writing function calls
--    from inner to outer (most nested to least nested).
-- 2. **Name Conversion:** Each `FunctionName` is converted to a C-compatible string using the `convertName`
--    helper function. This might involve replacing special characters or mangling names to avoid conflicts
--    with C keywords or reserved symbols.
-- 3. **Prefix and Intercalation:** The resulting list of converted names is prefixed with `"hs_"` and joined
--    together using underscores (`_`) as separators.
--
-- The final string represents the function path in a valid C code format.
--
formatFunctionPath :: FunctionPath -> CCode
formatFunctionPath (FunctionPath names) =
  names |> reverse |> map convertName |> ("hs_" :) |> intercalate "_"
  where
    convertName :: FunctionName -> CCode
    convertName = \case
      PlainFunction name -> foldMap convertChar name
      LambdaCaseFunction index -> "case_" ++ show index
      Entry -> "_entry"
      where
        convertChar :: Char -> String
        convertChar = \case
          '$' -> "_S_"
          '\'' -> "_t_"
          '_' -> "__"
          x -> pure x


-- | Function to generate a C code table name based on a function path.
--
-- The `generateTableName` function takes a `FunctionPath` as an argument and returns a C code string that
-- represents the name of a table associated with the function path. Tiny tinyGHC  uses tables during
-- code generation to store information about functions or data structures. This function generates a table
-- name by applying the following steps:
--
-- 1. **Format Path:** The function path is formatted using the `formatFunctionPath` function. This ensures
--    the sequence of function names is converted to a valid C code string format (see documentation for
--    `formatFunctionPath`).
-- 2. **Prepend Prefix:** The formatted path is prepended with the string `"table_for_"`. This prefix helps
--    identify the string as a table name associated with a specific function path.
--
-- The final string is a C code compatible table name that reflects the function path it represents.
--
generateTableName :: FunctionPath -> CCode
generateTableName = formatFunctionPath >>> ("table_for_" <>)

-- | Function to generate a C code table pointer name based on a function path.
--
-- The `generateTablePointerName` function is similar to `generateTableName` but generates a name for a table
-- pointer variable instead of a table name. Tiny tinyGHC might use pointers to reference tables during code
-- generation. This function generates a table pointer name by applying the following steps:
--
-- 1. **Format Path:** The function path is formatted using the `formatFunctionPath` function (see previous
--    documentation).
-- 2. **Prepend Prefix:** The formatted path is prepended with the string `"table_pointer_for_"`. This prefix
--    helps identify the string as a table pointer variable name associated with a specific function path.
--
-- The final string is a C code compatible table pointer variable name that reflects the function path it
-- represents.
generateTablePointerName :: FunctionPath -> CCode
generateTablePointerName = formatFunctionPath >>> ("table_pointer_for_" <>)


-- | Type representing the indentation level during C code generation.
--
-- The `Indent` type is a simple alias for `Int`. It represents the number of columns that the current code
-- generation is indented. Indentation is used to improve the readability and structure of the generated C code.
type Indent = Int

-- | Number of columns used for each indentation level.
--
-- The `indentAmount` constant defines the number of columns added for each level of indentation in the
-- generated C code. This value controls the overall indentation style.
indentAmount :: Indent
indentAmount = 2

-- | Data type representing the context during C code generation.
--
-- The `CompilationContext` data type encapsulates various aspects of the current state during C code
-- generation. This context is  passed around or stored during code generation to provide essential
-- information for different code generation functions.
--
-- **Fields:**
--
-- * `currentFunction`: This field stores the `FunctionPath` representing the current function being
--   processed. This helps track the current location within the nested function calls encountered in the
--   tinyHaskell AST.
-- * `currentIndent`: This field stores the current indentation level (`Indent`) used for the generated C code.
--   Proper indentation is maintained as code generation progresses.
-- * `globals`: This field is a `GlobalFunctionMap` that  stores information about global functions
--   encountered during code generation. The exact contents and structure of this map depend on the
--   implementation details of Tiny tinyGHC.
-- * `cafs`: This field is another `GlobalFunctionMap`, potentially storing information about closure-related
--   functions (functions with captured variables). Similar to `globals`, the specific details depend on
--   Tiny tinyGHC's implementation.
-- * `locationTable`: This field is a `CCodeLocationMap` that  maps tinyHaskell code locations (source code
--   positions) to their corresponding C code representations. This helps with debugging and potential
--   error reporting.
-- * `subFunctionTable`: This field is an `IntMap FunctionPath`. It's  a map that associates indices
--   with the full `FunctionPath` for sub-functions (functions nested within other functions). This mapping
--   helps identify and reference sub-functions during code generation.
--
-- The `CompilationContext` provides a central structure for managing various aspects of the code generation
-- process within Tiny tinyGHC.
data CompilationContext = CompilationContext
  { currentFunction :: FunctionPath,
    currentIndent :: Indent,
    globals :: GlobalFunctionMap,
    cafs :: GlobalFunctionMap,
    locationTable :: CCodeLocationMap,
    subFunctionTable :: IntMap FunctionPath
  }
  deriving (Show)

-- | The initial context used at the beginning of C code generation.
--
-- The `startingContext` value defines the initial state of the `CompilationContext` used during C code
-- generation. This context  holds various pieces of information that are needed throughout the process.
-- The initial values are:
--
-- * `currentFunction`: Empty `FunctionPath` (no function initially).
-- * `currentIndent`: 0 (no indentation initially).
-- * `globals`: Empty `GlobalFunctionMap` (no global functions encountered yet).
-- * `cafs`: Empty `GlobalFunctionMap` (no closure-related functions encountered yet).
-- * `locationTable`: Empty `CCodeLocationMap` (no source code location mappings yet).
-- * `subFunctionTable`: Empty `IntMap FunctionPath` (no sub-functions encountered yet).
--
-- This initial context provides a starting point for the code generation process, and it can be updated as
-- more information becomes available during code traversal.
startingContext :: CompilationContext
startingContext = CompilationContext mempty 0 mempty mempty mempty mempty

-- | Monad transformer for C code generation.
--
-- The `CCodeGenerator` newtype defines a monad transformer for C code generation. This is a powerful
-- construct that allows us to combine functionalities of different monads. In this case, the transformer
-- combines three monads:
--
-- 1. `ReaderT CompilationContext`: This monad provides access to a `CompilationContext` through the
--    `ReaderT` functionality. The code generation functions can access and manipulate the current context
--    during code generation.
-- 2. `Writer CCode`: This monad accumulates the generated C code using the `Writer` monad functionality.
--    Code generation functions can write C code statements and expressions using `tell`. The final
--    generated C code is obtained by running the `CCodeGenerator` computation and extracting the written
--    code using `runWriter`.
-- 3. `Monad`: This base monad provides the core functionality for sequencing computations and handling
--    monadic effects (like error handling, which might be relevant in some code generation scenarios).
--
-- By combining these monads, `CCodeGenerator` allows for a structured and efficient approach to C code
-- generation. It encapsulates context access, code generation, and potential future error handling within
-- a single monadic framework.
--
-- The `CCodeGenerator` type also derives several useful monad typeclasses:
--
-- * `Functor`: Allows basic application of functions within the monad.
-- * `Applicative`: Allows function application and lifting of values into the monad.
-- * `Monad`: Provides basic monadic sequencing capabilities.
-- * `MonadReader CompilationContext`: Enables access to the `CompilationContext` within computations.
-- * `MonadWriter CCode`: Enables writing C code using the `tell` function.
--
-- These typeclasses provide a rich set of tools for building robust and expressive code generation functions
-- in Tiny tinyGHC.
newtype CCodeGenerator a = CCodeGenerator (ReaderT CompilationContext (Writer CCode) a)
  deriving (Functor, Applicative, Monad, MonadReader CompilationContext, MonadWriter CCode)

-- | Run a C code generation computation with the starting context.
--
-- The `runCWriter` function takes a `CCodeGenerator` computation as input and returns a tuple containing
-- the result of the computation (`a`) and the generated C code (`CCode`). This function utilizes the
-- `runReaderT` and `runWriter` functions to execute the computation within the starting context (`startingContext`)
-- and extract both the result and the accumulated C code.
--
-- This function is  used at the end of the code generation process to obtain the final generated C code
-- and any additional results produced by the computation.
runCWriter :: CCodeGenerator a -> (a, CCode)
runCWriter (CCodeGenerator m) =
  runReaderT m startingContext |> runWriter

-- | Write a line of C code in the context
writeLine :: CCode -> CCodeGenerator ()
writeLine code = do
  amount <- asks currentIndent
  tell (replicate amount ' ')  -- Indent based on current level
  tell code
  tell "\n"

-- | Write a comment line in the generated C code.
--
-- The `comment` function writes a single line comment prefixed with "//" to the generated C code using
-- the `Writer` monad (`tell`). This function is  useful for adding comments to explain specific code
-- sections or document the generated C code. The comments can potentially be disabled by modifying how this
-- function is used within the code generation process.
comment :: CCode -> CCodeGenerator ()
comment code = writeLine ("// " <> code)

-- | Modify a C code generation computation to use increased indentation.
--
-- The `indented` function takes a `CCodeGenerator` computation as input and returns a new computation
-- that will use increased indentation during code generation. This function utilizes the `local` function
-- from the `ReaderT` monad to update the `currentIndent` field within the `CompilationContext`. The new
-- indentation level is calculated by adding the `indentAmount` (defined earlier) to the current indentation
-- stored in the context.
--
-- The modified computation within `do` block will then benefit from this increased indentation when writing
-- C code lines using `writeLine`. This helps create visually structured and well-formatted C code.
--
indented :: CCodeGenerator a -> CCodeGenerator a
indented =
  local (\r -> r {currentIndent = indentAmount + currentIndent r})

-- | Execute a C code generation computation within the scope of a named function.
--
-- The `insideFunction` function takes a `FunctionName` and a `CCodeGenerator` computation as arguments.
-- It returns a new computation that executes the provided computation within the context of the specified
-- function name. This is achieved by updating the `currentFunction` field in the `CompilationContext` using
-- the `local` function from the `ReaderT` monad. The new `currentFunction` is constructed by appending the
-- provided `FunctionName` to the current function path stored in the context.
--
-- By using this function, code generation can track the current function being processed and potentially
-- generate C code specific to that function's context. This is useful for managing nested function calls
-- and generating appropriate C code structures.
insideFunction :: FunctionName -> CCodeGenerator a -> CCodeGenerator a
insideFunction name =
  local (\r -> r {currentFunction = appendFunctionName name (currentFunction r)})

-- | Execute a C code generation computation with access to specific globals.
--
-- The `withGlobals` function takes a `GlobalFunctionMap` (map of global functions) and a
-- `CCodeGenerator` computation as arguments. It returns a new computation that executes the provided
-- computation with access to the specified set of global functions. This is achieved by updating the
-- `globals` field in the `CompilationContext` using the `local` function from the `ReaderT` monad. The new
-- context holds the provided `globals` map.
--
-- Additionally, the `withGlobals` function applies locations for these globals using the `applyLocations`
-- function (implementation not shown). This  involves generating C code expressions that access
-- the memory locations of the global functions.
--
withGlobals :: GlobalFunctionMap -> CCodeGenerator a -> CCodeGenerator a
withGlobals globals =
  local (\r -> r {globals = globals})
    >>> applyLocations impliedLocations
  where
    table (i, path) = createSingleLocation (Global i) (printf "(uint8_t*)&%s" (generateTablePointerName path))
    impliedLocations = globals |> IntMap.toList |> foldMap table

-- | Get the C code representation for a global function by its index.
--
-- The `retrieveGlobalFunction` function takes a `SubFunctionIndex` (index of a sub-function) and returns
-- a `CCodeGenerator` computation that produces the C code representation for the global function
-- associated with that index. This function performs the following steps:
--
-- 1. **Access Globals:** It uses `asks globals` to access the current `GlobalFunctionMap` from the context.
-- 2. **Find Function Path:** It looks up the function path for the given index using `IntMap.findWithDefault`.
--    If the index is not found, an error is raised using `error`.
-- 3. **Format Path:** If the function path is found, it's formatted into a valid C code string using
--    `formatFunctionPath`.
--
-- The final computation result is the C code representation of the global function.
retrieveGlobalFunction :: SubFunctionIndex -> CCodeGenerator CCode
retrieveGlobalFunction i = do
  globals' <- asks globals
  let path = IntMap.findWithDefault err i globals'
  return (formatFunctionPath path)
  where
    err = error ("Global Function " <> show i <> " does not exist.")

-- | Process closure-related functions (CAFs) with access to their locations.
--
-- The `processCafs` function takes a `GlobalFunctionMap` containing closure-related functions (CAFs) and
-- a `CCodeGenerator` computation as arguments. It returns a new computation that performs the following
-- steps:
--
-- 1. **Update Context:** It updates the `cafs` field in the `CompilationContext` using `local` to provide
--    access to the specified set of CAFs during code generation.
-- 2. **Apply Locations:** It applies locations for these CAFs using the `applyLocations` function
--    (implementation shown later). This  involves generating C code expressions that access the
--    memory locations of the CAF data structures.
processCafs :: GlobalFunctionMap -> CCodeGenerator a -> CCodeGenerator a
processCafs cafs =
  local (\r -> r {cafs = cafs}) >>> applyLocations impliedLocations
  where
    location (i, path) =
      createSingleLocation (CAF i) (printf "(uint8_t*)&%s" (generateCafCellIdentifier path))

    impliedLocations =
      cafs |> IntMap.toList |> foldMap location

-- | Execute a C code generation computation with access to additional code locations.
--
-- The `applyLocations` function takes a `CCodeLocationMap` (map of code locations) and a
-- `CCodeGenerator` computation as arguments. It returns a new computation that executes the provided
-- computation with access to the combined set of code locations. This function combines the new locations
-- with the existing locations stored in the context using the `<>` operator ( function concatenation).
--
-- The implementation detail of how locations are used to generate C code expressions is omitted for brevity.
applyLocations :: CCodeLocationMap -> CCodeGenerator a -> CCodeGenerator a
applyLocations newLocations =
  local (\r -> r {locationTable = newLocations <> locationTable r})

-- | Execute a C code generation computation while handling sub-functions.
--
-- The `handleSubFunctions` function takes a list of `LambdaFunction`s (representing sub-functions) and a
-- `CCodeGenerator` computation as arguments. It returns a new computation that performs the following steps:
--
-- 1. **Get Current Function:** It retrieves the current function path using `asks currentFunction`.
-- 2. **Construct Function Paths:** It constructs full function paths for each sub-function by
--    appending the sub-function's name to the current function path using `appendFunctionName`.
-- 3. **Create Index Map:** It creates an `IntMap` that associates indices (starting from 0) with the
--    constructed full function paths for the sub-functions.
-- 4. **Update Context:** It updates the `subFunctionTable` field in the `CompilationContext` using `local`
--    to provide access to the mapping between indices and sub-function paths.
-- 5. **Execute Computation:** Finally, it executes the provided computation (`m`) within this updated context.
handleSubFunctions :: [LambdaFunction] -> CCodeGenerator a -> CCodeGenerator a
handleSubFunctions functions m = do
  current <- asks currentFunction
  let makePath LambdaFunction {..} = appendFunctionName functionName current
      table = functions |> map makePath |> zip [0 ..] |> IntMap.fromList
  local (\r -> r {subFunctionTable = table}) m

-- | Retrieve the full function path for a sub-function by its index.
--
-- The `fetchSubFunctionPath` function takes a `SubFunctionIndex` (index of a sub-function) and returns a
-- `CCodeGenerator` computation that produces the full function path for the sub-function associated with
-- that index. This function performs the following steps:
--
-- 1. **Access Sub-Function Table:** It uses `asks subFunctionTable` to access the current mapping between
--    indices and sub-function paths stored in the context.
-- 2. **Find Path:** It looks up the function path for the given index using `IntMap.findWithDefault`. If the
--    index is not found, an error is raised using `error`.
--
-- The final computation result is the full function path for the sub-function.
fetchSubFunctionPath :: SubFunctionIndex -> CCodeGenerator FunctionPath
fetchSubFunctionPath n =
  asks (subFunctionTable >>> IntMap.findWithDefault err n)
  where
    err = error ("Sub Function " <> show n <> " has no C function associated with it")

-- | Get the C code representation for a sub-function by its index.
--
-- The `retrieveSubFunction` function takes a `SubFunctionIndex` (index of a sub-function) and returns a
-- `CCodeGenerator` computation that produces the C code representation for the sub-function associated with
-- that index. This function performs the following steps:
--
-- 1. **Fetch Path:** It uses `fetchSubFunctionPath` to retrieve the full function path for the sub-function.
-- 2. **Format Path:** It formats the retrieved function path into a valid C code string using
--    `fmap formatFunctionPath`.
--
-- The final computation result is the C code representation of the sub-function.
retrieveSubFunction :: SubFunctionIndex -> CCodeGenerator CCode
retrieveSubFunction = fetchSubFunctionPath >>> fmap formatFunctionPath

-- | Get the table name for a sub-function by its index.
--
-- The `fetchTableName` function takes a `SubFunctionIndex` (index of a sub-function) and returns a
-- `CCodeGenerator` computation that produces the C code table name associated with the sub-function. This
-- function  relies on the naming convention used for sub-function tables during code generation.
--
-- 1. **Fetch Path:** It uses `fetchSubFunctionPath` to retrieve the full function path for the sub-function.
-- 2. **Generate Table Name:** It uses `fmap generateTableName` to generate the table name based on the
--    retrieved function path using the `generateTableName` function (assumed to be defined elsewhere).
--
-- The final computation result is the C code table name for the sub-function.
fetchTableName :: SubFunctionIndex -> CCodeGenerator CCode
fetchTableName = fetchSubFunctionPath >>> fmap generateTableName

-- | Get the C code expression to access a specific location.
--
-- The `locateCCode` function takes a `Location` and returns a `CCodeGenerator` computation that produces the
-- C code expression for accessing the memory location represented by that `Location`. This function leverages
-- the information stored in the `locationTable` within the `CompilationContext`.
--
-- 1. **Access Location Table:** It uses `asks locationTable` to access the current mapping between
--    locations and C code expressions stored in the context.
-- 2. **Find Expression:** It looks up the C code expression for the given location using
--    `findCCodeLocation`. If the location is not found, an error is raised using `error`.
-- 3. **Wrap and Extract:** The retrieved expression is wrapped in a `Maybe` to handle potential lookup failures.
--    The `fromMaybe` function then extracts the expression or raises an error if not found.
--
-- The final computation result is the C code expression for accessing the memory location.
locateCCode :: Location -> CCodeGenerator CCode
locateCCode location = do
  table <- asks locationTable
  return (fromMaybe err (findCCodeLocation table location))
  where
    err = error ("could not find C location for " ++ show location)

-- | Get the C code expression to access a CAF cell by its index.
--
-- The `fetchCafCell` function takes a `SubFunctionIndex` (index of a CAF) and returns a `CCodeGenerator`
-- computation that produces the C code expression for accessing the cell associated with that CAF. This
-- function performs the following steps:
--
-- 1. **Access CAF Map:** It uses `asks cafs` to access the current map of closure-related functions (CAFs)
--    stored in the context.
-- 2. **Find CAF:** It looks up the CAF for the given index using `IntMap.findWithDefault`. If the index is
--    not found, an error is raised using `error`.
-- 3. **Generate Identifier:** It uses `generateCafCellIdentifier` (assumed to be defined elsewhere) to
--    generate the C code identifier for the CAF cell based on the retrieved CAF information.
--
-- The final computation result is the C code expression for accessing the CAF cell.
fetchCafCell :: SubFunctionIndex -> CCodeGenerator CCode
fetchCafCell n = asks (cafs >>> IntMap.findWithDefault err n >>> generateCafCellIdentifier)
  where
    err = error ("CAF " <> show n <> " has no cell associated with it")


-- | Traverse the C minus minus (Cmm) representation to gather all global functions.
--
-- The `collectGlobalFunctions` function takes a `Cmm` data structure (representing the Core Mini-Language
-- intermediate representation) as input and returns a `CCodeGenerator` computation that produces a tuple of
-- two `GlobalFunctionMap`s. This function performs the following steps:
--
-- 1. **Combine Entry and Functions:** It combines the `entry` point ( a special function) with the
--    provided list of `LambdaFunction`s (representing functions) using `gatherInFunctions`.
--
-- **Inner Function: gatherInFunctions**
--
-- The `gatherInFunctions` function takes a list of `LambdaFunction`s and returns a `CCodeGenerator` computation
-- that accumulates mappings for global functions. It uses `foldMapM` (monadic fold) to process the list.
--
-- **Inner Function: gatherInFunction**
--
-- The `gatherInFunction` function takes a single `LambdaFunction` and returns a `CCodeGenerator` computation
-- that contributes to the overall mapping of global functions. Here's what it does:
--
-- 1. **Enter Function Context:** It uses `insideFunction functionName` to enter the context of the current
--    function being processed.
-- 2. **Get Current Function Path:** It retrieves the current function path using `asks currentFunction`.
-- 3. **Determine Closure Type Mapping:** It determines the appropriate mapping based on the `closureType` of the
--    function:
--     - `DynamicClosure`: No mapping is created (dynamic closures don't have unique indices).
--     - `GlobalClosure i`: A mapping for this global closure is created using `IntMap.singleton`. The key is
--       the index `i` and the value is the current function path.
--     - `CAFClosure i`: A mapping for this CAF closure is created using `IntMap.singleton`. The key is the
--       index `i` and the value is the current function path.
-- 4. **Gather Sub-Function Mappings:** It recursively calls `gatherInFunctions` on the `subFunctions` list to
--    gather mappings for any nested functions within this function.
-- 5. **Combine Mappings:** Finally, it combines the mapping for the current function (created in step 3) with
--    the mappings for sub-functions (gathered in step 4) using the `<>` operator ( function concatenation).
--
-- The final result of the `collectGlobalFunctions` computation is a tuple containing two `GlobalFunctionMap`s.
-- The first map holds information about global functions, and the second map holds information about CAF closures.
collectGlobalFunctions :: Cmm -> CCodeGenerator (GlobalFunctionMap, GlobalFunctionMap)
collectGlobalFunctions (Cmm functions entry) = gatherInFunctions (entry : functions)
  where
    gatherInFunctions :: [LambdaFunction] -> CCodeGenerator (GlobalFunctionMap, GlobalFunctionMap)
    gatherInFunctions = foldMapM gatherInFunction

    gatherInFunction :: LambdaFunction -> CCodeGenerator (GlobalFunctionMap, GlobalFunctionMap)
    gatherInFunction LambdaFunction {..} =
      insideFunction functionName <| do
        current <- asks currentFunction
        let thisMapping = case closureType of
              DynamicClosure -> mempty
              GlobalClosure i -> (IntMap.singleton i current, mempty)
              CAFClosure i -> (mempty, IntMap.singleton i current)
        thoseMappings <- gatherInFunctions subFunctions
        return (thisMapping <> thoseMappings)


-- | Generate C code for a block of instructions.
--
-- The `generateInstructionSet` function takes a `CodeBlock` (representing a block of instructions) as input and
-- returns a `CCodeGenerator` computation that produces no return value (unit type `()`). This function assumes
-- that all the necessary locations for accessing data (e.g., registers, memory) have already been established
-- during previous stages of code generation.
--
-- The function handles different cases based on the presence or absence of instructions in the block:
--
-- - Empty Block: If the code block is empty (`[]`), it simply writes a line returning `NULL`. This might be
--   used to signal the end of a function body with no explicit return value.
-- - PopExcessConstructorArgs: If the only instruction is `PopExcessConstructorArgs`, it also writes a line
--   returning `NULL`. This  handles a specific case related to constructor arguments in tinyHaskell.
-- - Non-Empty Block: If the code block contains actual instructions, the function iterates over each instruction
--   using `forM_`. For each instruction:
--     - A comment is added using `comment` to document the instruction for readability.
--     - The `genInstr` function is called to generate the C code for the specific instruction.
--
-- **Inner Function: genInstr**
--
-- The `genInstr` function takes an `Instruction` and is responsible for generating the corresponding C code snippet.
-- It uses pattern matching on the instruction type to handle different instruction categories:
--
-- - Binary Instructions (genB2): These instructions operate on two operands. The function uses `printf` formatted
--   strings to generate C code for arithmetic operations (`Add2`, `Sub2`, `Mul2`, `Div2`), comparisons
--   (`Less2`, `LessEqual2`, `Greater2`, `GreaterEqual2`, `EqualTo2`, `NotEqualTo2`), and string concatenation
--   (`Concat2`). The operands (`l1` and `l2`) are assumed to be C code expressions representing locations or values.
-- - Unary Instructions (genB1): These instructions operate on a single operand. The function uses `printf` formatted
--   strings to generate C code for printing an integer (`PrintInt1`) or a string (`PrintString1`). The operand (`l`)
--   is assumed to be a C code expression representing a location or value. The `PrintString1` case requires an
--   adjustment to skip the `InfoTable*` pointer at the beginning of the string data structure.
--   (This is  a tinyGHC-specific detail related to how string data is stored.)
--
-- The `genInstr` function is a helper function used within `generateInstructionSet` to handle individual
-- instructions. It takes an `Instruction` as input and performs the following actions based on the instruction type:
--
-- - Store Instructions:
--   - `StoreInt location`: Retrieves the C code expression for the `location` using `locateCCode`. Then, it writes
--     a line using `printf` to store the value in the `g_IntRegister` register.
--   - `StoreString location`: Similar to `StoreInt`, it retrieves the C code expression for the `location` and writes
--     a line using `printf` to store the value in the `g_StringRegister` register.
-- - Tag Instructions:
--   - `StoreTag tag`: Writes a line using `printf` to store the provided `tag` value in the `g_TagRegister` register.
--   - `StoreConstructorArgCount count`: Writes a line using `printf` to store the `count` value in the
--     `g_ConstructorArgCountRegister` register.
-- - Argument Stack (SA) Instructions:
--   - `PopExcessConstructorArgs`: Adjusts the top of the argument stack (`g_SA`) by subtracting the
--     `g_ConstructorArgCountRegister` value.
-- - Function Calls (Enter):
--   - `Enter (Global i)`: Retrieves the C code for the global function with index `i` using `retrieveGlobalFunction`.
--     Then, it writes a line using `printf` to return the address of that function using the retrieved C code expression.
--   - `Enter location`: Retrieves the C code expression for the `location` using `locateCCode`. Then, it writes lines
--     to store the location in the `g_NodeRegister` register and to follow the pointer to access the actual data structure
--     using `read_info_table`.
--   - `EnterCaseContinuation`: Writes comments to indicate the frame manipulation ( for debugging purposes).
--     Then, it retrieves the C code for the code pointer stored at the top of the evaluation stack (`g_SB`) and returns it.
-- - Exit Instruction:
--   - `Exit`: Writes a line to return `NULL`.
-- - Built-in Function Calls:
--   - `BuiltinFunction2 b location1 location2`: Retrieves C code expressions for `location1` and `location2` using
--     `locateCCode`. Then, it calls `genB2` to generate code for the built-in binary function `b` using the retrieved expressions.
--   - `BuiltinFunction1 b location`: Retrieves a C code expression for `location` using `locateCCode`. Then, it calls
--     `genB1` to generate code for the built-in unary function `b` using the retrieved expression.
-- - Argument Stack (SA) Instructions (continued):
--   - `PushSA location`: Retrieves the C code expression for the `location` using `locateCCode`. Then, it writes lines
--     to store the value in the first element of the argument stack (`g_SA`) and increment the stack pointer.
--   - `PushConstructorArg location`: Similar to `PushSA`, it retrieves the C code expression for the `location` and stores
--     the value in the argument stack, but it might be used specifically for constructor arguments.
--   - `PushCaseContinuation index`: Retrieves the C code for the sub-function at index `index` using `retrieveSubFunction`.
--     Then, it writes lines to store the code pointer of the sub-function in the first element of the evaluation stack
--     (`g_SB`) and increment the stack pointer.
-- - Bury Instructions:
--   - `Bury location`: Retrieves the C code expression for the `location` using `locateCCode`. Then, it writes lines
--     to store the value in the argument stack (`g_SA`) and increment the stack pointer.
--   - `BuryInt location`, `BuryString location`: Similar to `Bury location`, but they store an integer or string value
--     explicitly retrieved from the location.
-- - Memory Allocation Instructions:
--   - `AllocTable index`: Allocates a table on the heap. It generates C code to:
--     1. Get a variable name for the allocated table using `generateAllocatedVariable`.
--     2. Fetch the table name using `fetchTableName`.
--     3. Write C code to allocate memory using `heap_cursor` and initialize the info table using
--        `heap_write_info_table`.
--   - `AllocPointer location`: Allocates a pointer on the heap. It retrieves the C code expression for the
--     location (`l`) using `locateCCode` and writes C code to allocate a pointer using `heap_write_ptr`.
--   - `AllocBlankPointer`: Allocates a null pointer on the heap. It writes C code using `heap_write_ptr` with
--     `NULL` as the argument.
--   - `AllocInt location`: Allocates an integer on the heap. It retrieves the C code expression for the
--     location (`l`) using `locateCCode` and writes C code to allocate an integer using `heap_write_int`.
--   - `AllocString location`: Allocates a string on the heap. It retrieves the C code expression for the
--     location (`l`) using `locateCCode` and writes C code to allocate a pointer using `heap_write_ptr`.
--
-- - Printing Instruction:
--   - `PrintError s`: Prints an error message to the console. It uses `printf` with a formatted string to print
--     the provided error message (`s`).
--
-- - Control Flow Instruction:
--   - `PushUpdate`: Pushes an update frame onto the stack. It  relates to handling updates within
--     case expressions in tinyHaskell. It writes C code to:
--     1. Save the current static and activation links (`save_SB` and `save_SA`).
--     2. Set the closure of the top frame to the current node register (`g_NodeRegister`).
--     3. Set the code pointer of the top frame to the `update_constructor` function ( defined elsewhere).
--     4. Increment the stack pointer (`g_SB.top`).
--
-- - Closure Creation Instruction:
--   - `CreateCAFClosure index`: Creates a closure for a CAF (closure-related function) on the heap. It performs
--     the following steps:
--     1. Fetch the C code expression for the CAF cell using `fetchCafCell`.
--     2. Write C code to update the `g_CAFListLast` pointer to point to the newly created CAF cell.
--     3. Allocate memory for the CAF closure using `heap_cursor`.
--     4. Set the `g_NodeRegister` to point to the allocated closure.
--     5. Initialize the info table of the closure with the `table_for_black_hole` (assumed to be defined elsewhere).
--     6. Write C code to allocate a null pointer for padding purposes (potentially related to memory alignment).
generateInstructionSet :: CodeBlock -> CCodeGenerator ()
generateInstructionSet (CodeBlock _ _ []) = writeLine "return NULL;"
generateInstructionSet (CodeBlock _ _ [PopExcessConstructorArgs]) = writeLine "return NULL;"
generateInstructionSet (CodeBlock _ _ instrs) =
  forM_ instrs <| \instr -> do
    genInstr instr
  where
    genB1 b l = case b of
      PrintInt1 -> writeLine (printf "printf(\"%%ld\\n\", %s);" l)
      PrintString1 ->
        writeLine (printf "printf(\"%%s\\n\", (char*)(%s + sizeof(InfoTable*)));" l)
      Negate1 -> writeLine (printf "g_IntRegister = -%s;" l)

    genB2 b l1 l2 = case b of
      Add2 -> writeLine (printf "g_IntRegister = %s + %s;" l1 l2)
      Sub2 -> writeLine (printf "g_IntRegister = %s - %s;" l1 l2)
      Mul2 -> writeLine (printf "g_IntRegister = %s * %s;" l1 l2)
      Div2 -> writeLine (printf "g_IntRegister = %s / %s;" l1 l2)
      Less2 -> writeLine (printf "g_IntRegister = %s < %s;" l1 l2)
      LessEqual2 -> writeLine (printf "g_IntRegister = %s <= %s;" l1 l2)
      Greater2 -> writeLine (printf "g_IntRegister = %s > %s;" l1 l2)
      GreaterEqual2 -> writeLine (printf "g_IntRegister = %s >= %s;" l1 l2)
      EqualTo2 -> writeLine (printf "g_IntRegister = %s == %s;" l1 l2)
      NotEqualTo2 -> writeLine (printf "g_IntRegister = %s /= %s;" l1 l2)
      Concat2 -> writeLine (printf "g_StringRegister = string_concat(%s, %s);" l1 l2)

    genInstr = \case
      StoreInt location ->
        locateCCode location >>= \l ->
          writeLine (printf "g_IntRegister = %s;" l)
      StoreString location ->
        locateCCode location >>= \l ->
          writeLine (printf "g_StringRegister = %s;" l)
      StoreTag tag -> writeLine (printf "g_TagRegister = %d;" tag)
      StoreConstructorArgCount count ->
        writeLine (printf "g_ConstructorArgCountRegister = %d;" count)
      PopExcessConstructorArgs ->
        writeLine "g_SA.top -= g_ConstructorArgCountRegister;"
      Enter (Global i) ->
        retrieveGlobalFunction i >>= \l ->
          writeLine (printf "return &%s;" l)
      Enter location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_NodeRegister = %s;" l)
          writeLine (printf "return read_info_table(%s)->entry;" l)
      EnterCaseContinuation -> do
        writeLine "--g_SB.top;"
        writeLine "return g_SB.top[0].as_code;"
      Exit -> writeLine "return NULL;"
      BuiltinFunction2 b location1 location2 -> do
        l1 <- locateCCode location1
        l2 <- locateCCode location2
        genB2 b l1 l2
      BuiltinFunction1 b location -> locateCCode location >>= genB1 b
      PushSA location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      PushConstructorArg location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      PushCaseContinuation index -> do
        function <- retrieveSubFunction index
        writeLine (printf "g_SB.top[0].as_code = &%s;" function)
        writeLine "++g_SB.top;"
      Bury location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      BuryInt location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_SB.top[0].as_int = %s;" l)
          writeLine "++g_SB.top;"
      BuryString location ->
        locateCCode location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      AllocTable index -> do
        let var = generateAllocatedVariable index
        table <- fetchTableName index
        writeLine (printf "uint8_t* %s = heap_cursor();" var)
        writeLine (printf "heap_write_info_table(&%s);" table)
      AllocPointer location ->
        locateCCode location >>= \l ->
          writeLine (printf "heap_write_ptr(%s);" l)
      AllocBlankPointer -> do
        writeLine (printf "heap_write_ptr(NULL);")
      AllocInt location ->
        locateCCode location >>= \l ->
          writeLine (printf "heap_write_int(%s);" l)
      AllocString location ->
        locateCCode location >>= \l ->
          writeLine (printf "heap_write_ptr(%s);" l)
      PrintError s ->
        writeLine (printf "puts(\"Error:\\n%s\");" s)
      PushUpdate -> do
        writeLine "save_SB();"
        writeLine "save_SA();"
        writeLine "g_SB.top[0].as_closure = g_NodeRegister;"
        writeLine "g_SB.top[1].as_code = &update_constructor;"
        writeLine "g_SB.top += 2;"
      CreateCAFClosure index -> do
        cell <- fetchCafCell index
        writeLine (printf "*g_CAFListLast = &%s;" cell)
        writeLine (printf "g_CAFListLast = &%s.next;" cell)
        writeLine (printf "%s.closure = heap_cursor();" cell)
        writeLine (printf "g_NodeRegister = %s.closure;" cell)
        writeLine "heap_write_info_table(&table_for_black_hole);"
        writeLine "heap_write_ptr(NULL);"


-- | Generate C code for the body of a normal function.
--
-- The `produceNormalFunctionBody` function takes the following arguments:
--
-- - `argumentsCount`: The number of arguments the function expects.
-- - `bound`: Information about bound variables (environment) for the function.
-- - `body`: The `CodeBlock` representing the function's body instructions.
--
-- This function returns a `CCodeGenerator` computation that produces no return value (unit type `()`). It performs
-- the following steps to generate C code for the function body:
--
-- 1. **Allocate Heap Space:** It calls `allocateHeapSpace` (function assumed to be defined elsewhere) to potentially
--    allocate memory on the heap for the function's execution. This might be necessary for data structures or temporary
--    values used within the function.
-- 2. **Pop Arguments:** It calls `popArgs` to handle function arguments from the stack. This is only done if there
--    are any arguments (`argumentsCount > 0`). The `popArgs` function is explained in detail later.
-- 3. **Pop Bound Variables:** It calls `popBound` to handle bound variables (environment) for the function. This
--    extracts information about bound pointers, integers, and strings from the closure environment and stores them
--    in temporary variables. The `popBound` function is explained in detail later.
-- 4. **Apply Locations:** It combines the argument and bound variable locations (obtained from `popArgs` and `popBound`)
--    using `manyLocations` and applies these locations to the code generation for the function body using
--    `applyLocations`. This ensures that the generated C code uses the correct memory locations to access arguments
--    and bound variables during function execution. The `applyLocations` function is assumed to be defined elsewhere.
--    The actual function body instructions are generated by `generateInstructionSet`.
--
-- **Inner Function: popArgs**
--
-- The `popArgs` function handles popping arguments from the stack:
--
-- - It adds a comment to document this step.
-- - It iterates over the argument indices (`[1 .. argumentsCount]`) using `forM_`. For each argument:
--     - A variable name is generated using `generateArgVariable`.
--     - A C code line is written to store the argument value from the stack into the generated variable. This uses
--       `printf` formatting to construct the C code expression.
-- - Finally, it writes a C code line to adjust the stack pointer based on the number of popped arguments.
-- - It returns a `CCodeLocationMap` representing the locations for the arguments (stored in the generated variables).
--
-- **Inner Function: popBound**
--
-- The `popBound` function handles extracting information about bound variables from the closure environment:
--
-- - It checks if there are any bound variables based on the provided `ArgumentInfo`. If none (`ArgumentInfo {..} = 0 0 0`),
--   it simply returns an empty `CCodeLocationMap`.
-- - It adds a comment to document this step.
-- - It declares a temporary variable named `temporaryClosurePointer` to hold the pointer to the closure environment
--   data structure.
-- - It uses `fold <$> sequence` to combine the results of three monadic computations (`popPointers`, `popInts`,
--   `popStrings`) that handle bound pointers, integers, and strings respectively (explained below).
--
-- **Inner Functions for Bound Variables**
--
-- - `popPointers`: If there are bound pointers (`boundPointers > 0`), it iterates over the pointer indices and:
--     - Generates a variable name using `generateBoundPointerVariable`.
--     - Writes C code lines to:
--       - Read the pointer value from the closure environment using `read_ptr`.
--       - Update the `temporaryClosurePointer` to point to the next element in the environment.
-- - It returns a `CCodeLocationMap` representing the locations for the bound pointers (stored in the generated variables).
--
-- - `popInts` and `popStrings` follow a similar structure as `popPointers` to handle bound integers and strings
--   respectively. They use `read_int` for integers and `read_ptr` for strings (assuming string data pointers).
produceNormalFunctionBody :: Int -> ArgumentInfo -> CodeBlock -> CCodeGenerator ()
produceNormalFunctionBody argumentsCount bound body = do
  allocateHeapSpace body
  arguments <- if argumentsCount <= 0 then return mempty else popArgs
  boundArguments <- popBound bound
  applyLocations (arguments <> boundArguments) (generateInstructionSet body)
  where
    popArgs :: CCodeGenerator CCodeLocationMap
    popArgs = do
      pairs <-
        forM [1 .. argumentsCount] <| \n -> do
          let var = generateArgVariable n
          writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
          return (Argument (n - 1), var)
      writeLine (printf "g_SA.top -= %d;" argumentsCount)
      return (manyLocations pairs)

    popBound :: ArgumentInfo -> CCodeGenerator CCodeLocationMap
    popBound (ArgumentInfo 0 0 0) = return mempty
    popBound ArgumentInfo {..} = do
      writeLine (printf "uint8_t* %s = g_NodeRegister + sizeof(InfoTable*);" temporaryClosurePointer)
      fold <$> sequence [popPointers, popInts, popStrings]
      where
        popPointers = case boundPointers of
          0 -> return mempty
          count -> do
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = generateBoundPointerVariable n
                writeLine (printf "uint8_t* %s = read_ptr(%s);" var temporaryClosurePointer)
                writeLine (printf "%s += sizeof(uint8_t*);" temporaryClosurePointer)
                return (Bound n, var)
            return (manyLocations pairs)
        popInts = case boundInts of
          0 -> return mempty
          count -> do
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = generateBoundIntVariable n
                writeLine (printf "int64_t %s = read_int(%s);" var temporaryClosurePointer)
                writeLine (printf "%s += sizeof(int64_t);" temporaryClosurePointer)
                return (BoundInt n, var)
            return (manyLocations pairs)
        popStrings = case boundStrings of
          0 -> return mempty
          count -> do
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = generateBoundStringVariable n
                writeLine (printf "uint8_t* %s = read_ptr(%s);" var temporaryClosurePointer)
                writeLine (printf "%s += sizeof(uint8_t*);" temporaryClosurePointer)
                return (BoundString n, var)
            return (manyLocations pairs)

-- | Allocate memory on the heap for a code block.
--
-- The `allocateHeapSpace` function takes a `CodeBlock` (representing a block of instructions) as input and returns a
-- `CCodeGenerator` computation that produces no return value (unit type `()`). This function analyzes the code block
-- to determine the amount of memory required on the heap and generates C code to reserve that space.
--
-- The function handles two cases based on the presence or absence of memory allocation information in the code block:
--
-- - No Allocation: If the `CodeBlock` does not contain any memory allocation information (`alloc == mempty`), the
--   function simply returns without any code generation. This indicates that the code block does not require any
--   heap allocation.
-- - Memory Allocation: If the `CodeBlock` contains memory allocation information (`MemoryAllocation {...}`), the
--   function performs the following steps:
--     - A comment is added to document the allocation process.
--     - A variable named `calculateAllocationSize` (assumed to be defined elsewhere) is used to calculate the total
--       amount of memory required based on the allocation information. The calculation  involves summing up the
--       memory requirements for tables, pointers, integers, and strings.
--       - The C code to declare this variable and initialize it to zero is written using `printf`.
--     - The `addSize` function (explained later) is called multiple times to accumulate the memory required for
--       different categories: tables, pointers, integers, and strings. These categories  correspond to different
--       data structures used within the code block.
--     - Finally, the C code to reserve the calculated amount of memory on the heap is written using `heap_reserve`
--       (assumed to be a function provided by the runtime system).
--
-- **Inner Function: addSize**
--
-- The `addSize` function is a helper function used to accumulate memory requirements for different categories within
-- the total allocation size:
--
-- - It takes three arguments:
--     - `cmt`: A comment string to document the category being added.
--     - `sizeof`: A string representing the size of a single element in the category (e.g., "sizeof(InfoTable*)").
--     - `count`: The number of elements in the category (obtained from the memory allocation information).
-- - It handles two cases based on the element count:
--     - Zero Count: If `count` is zero, the function simply returns without any code generation. This indicates
--       no elements of that category need allocation.
--     - Non-Zero Count: If `count` is greater than zero, the function performs the following steps:
--       - A comment is added using the provided `cmt` string.
--       - The C code to update the `calculateAllocationSize` variable is written using `printf`. This expression adds
--         the product of `count` and `sizeof` to the current value of `calculateAllocationSize`. This effectively
--         accumulates the memory requirement for the specific category.
allocateHeapSpace :: CodeBlock -> CCodeGenerator ()
allocateHeapSpace (CodeBlock alloc _ _) | alloc == mempty = return ()
allocateHeapSpace (CodeBlock MemoryAllocation {..} _ _) = do
  writeLine (printf "size_t %s = 0;" calculateAllocationSize)
  addSize "table allocations" "sizeof(InfoTable*)" tablesAllocated
  addSize "pointer allocations" "sizeof(uint8_t*)" pointersAllocated
  addSize "int allocations" "sizeof(int64_t)" intsAllocated
  addSize "string allocations" "sizeof(uint8_t*)" stringsAllocated
  writeLine (printf "heap_reserve(%s);\n" calculateAllocationSize)
  where
    addSize _ _ 0 = return ()
    addSize cmt sizeof count = do
      writeLine (printf "%s += %d * %s;" calculateAllocationSize count sizeof)

-- | Generate C code for the body of a continuation.
--
-- The `produceContinuationBody` function takes the following arguments:
--
-- - `buriedArgs`: Information about buried arguments (environment) for the continuation.
-- - `body`: The `CodeBlock` representing the continuation's body instructions.
--
-- This function returns a `CCodeGenerator` computation that produces no return value (unit type `()`). It performs
-- the following steps to generate C code for the continuation body:
--
-- 1. **Allocate Heap Space:** It calls `allocateHeapSpace` (function assumed to be defined elsewhere) to potentially
--    allocate memory on the heap for the continuation's execution. This might be necessary for data structures or
--    temporary values used within the continuation.
-- 2. **Pop Constructor Arguments:** It calls `popConstructorArgs` to handle arguments specific to the constructor
--    being applied in the continuation. These arguments are  on the stack and need to be accessed.
--    The `popConstructorArgs` function is explained in detail later.
-- 3. **Pop Buried Arguments:** It calls `popBuriedArgs` to handle "buried" arguments (environment) associated
--    with the continuation. These arguments are from previous function calls and might be stored on the stack or
--    in a separate stack for buried values. The `popBuriedArgs` function is explained in detail later.
-- 4. **Apply Locations:** It combines the constructor argument and buried argument locations (obtained from
--    `popConstructorArgs` and `popBuriedArgs`) using `manyLocations` and applies these locations to the code generation
--    for the continuation body using `applyLocations`. This ensures that the generated C code uses the correct memory
--    locations to access arguments and environment variables during continuation execution. The `applyLocations` function
--    is assumed to be defined elsewhere. The actual continuation body instructions are generated by
--    `generateInstructionSet`.
--
-- **Inner Function: popConstructorArgs**
--
-- The `popConstructorArgs` function handles popping constructor arguments from the stack:
--
-- - It checks if there are any constructor arguments (`CodeBlock _ 0 _`). If none, it simply returns an empty
--   `CCodeLocationMap`.
-- - It adds a comment to document this step.
-- - It iterates over the argument indices (`[1 .. count]`) using `forM_`. For each argument:
--     - A variable name is generated using `generateConstructorArgVariable`.
--     - A C code line is written to store the argument value from the stack into the generated variable. This uses
--       `printf` formatting to construct the C code expression.
-- - Finally, it writes a C code line to adjust the stack pointer based on the number of popped arguments.
-- - It returns a `CCodeLocationMap` representing the locations for the constructor arguments (stored in the generated variables).
--
-- **Inner Function: popBuriedArgs**
--
-- The `popBuriedArgs` function handles retrieving "buried" arguments (environment) associated with the continuation:
--
-- - It checks if there are any buried arguments based on the provided `ArgumentInfo`. If none (`ArgumentInfo {..} = 0 0 0`),
--   it simply returns an empty `CCodeLocationMap`.
-- - It adds a comment to document this step.
-- - It uses `sequence` to combine the results of three monadic computations (`popPointers`, `popInts`, `popStrings`)
--   that handle buried pointers, integers, and strings respectively (explained below). These computations are then
--   combined using `fmap fold` to produce a single `CCodeLocationMap`.
--
-- **Inner Functions for Buried Arguments**
--
-- - `popPointers`: If there are buried pointers (`boundPointers > 0`), it iterates over the pointer indices and:
--     - Generates a variable name using `generateBuriedPointerVariable`.
--     - Writes C code lines to:
--       - Read the pointer value from the stack ( the stack for buried values).
--       - Update the stack pointer based on the number of popped pointers.
-- - It returns a `CCodeLocationMap` representing the locations for the buried pointers (stored in the generated variables).
--
-- - `popInts` and `popStrings` follow a similar structure as `popPointers` to handle buried integers and strings
--   respectively. They use different stack access methods ( `g_SB.top` for buried values) based on their data types.
produceContinuationBody :: ArgumentInfo -> CodeBlock -> CCodeGenerator ()
produceContinuationBody buriedArgs body = do
  allocateHeapSpace body
  arguments <- popConstructorArgs body
  buried <- popBuriedArgs buriedArgs
  applyLocations (arguments <> buried) (generateInstructionSet body)
  where
    popConstructorArgs :: CodeBlock -> CCodeGenerator CCodeLocationMap
    popConstructorArgs (CodeBlock _ 0 _) = return mempty
    popConstructorArgs (CodeBlock _ count _) = do
      -- comment "popping constructor arguments"
      pairs <-
        forM [1 .. count] <| \n -> do
          let var = generateConstructorArgVariable n
          writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
          return (ConstructorArgument (n - 1), var)
      writeLine (printf "g_SA.top -= %d;" count)
      return (manyLocations pairs)
    popBuriedArgs :: ArgumentInfo -> CCodeGenerator CCodeLocationMap
    popBuriedArgs (ArgumentInfo 0 0 0) = return mempty
    popBuriedArgs ArgumentInfo {..} = do
      sequence
        [ popPointers boundPointers,
          popInts boundInts,
          popStrings boundStrings
        ]
        |> fmap fold
      where
        popPointers 0 = return mempty
        popPointers count = do
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = generateBuriedPointerVariable n
              writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
              return (Buried (n - 1), var)
          writeLine (printf "g_SA.top -= %d;" count)
          return (manyLocations pairs)
        popInts 0 = return mempty
        popInts count = do
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = generateBuriedIntVariable n
              writeLine (printf "int64_t %s = g_SB.top[-%d].as_int;" var n)
              return (BuriedInt (n - 1), var)
          writeLine (printf "g_SB.top -= %d;" count)
          return (manyLocations pairs)
        popStrings 0 = return mempty
        popStrings count = do
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = generateBuriedStringVariable n
              writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
              return (BuriedString (n - 1), var)
          writeLine (printf "g_SA.top -= %d;" count)
          return (manyLocations pairs)

-- | Initialize C code for handling constructor updates in a case expression.
--
-- The `initializeCaseHandling` function takes a `CCode` expression (`updateWith`) as input and returns a
-- `CCodeGenerator` computation that produces no return value (unit type `()`). This function generates C code to
-- check if a constructor update has been registered (using `g_ConstrUpdateRegister`). If an update is present,
-- the provided `updateWith` expression is executed ( to perform the update logic), and the
-- `g_ConstrUpdateRegister` is cleared. This mechanism ensures that constructor updates are handled only once
-- during case analysis.
initializeCaseHandling :: CCode -> CCodeGenerator ()
initializeCaseHandling updateWith = do
  writeLine "if (g_ConstrUpdateRegister != NULL) {"
  indented <| do
    writeLine updateWith
    writeLine "g_ConstrUpdateRegister = NULL;"
  writeLine "}"

-- | Generate C code for handling integer case expressions.
--
-- The `handleIntegerCases` function takes the following arguments:
--
-- - `buriedArgs`: Information about buried arguments (environment) for the case expression.
-- - `updateWith`: A `CCode` expression to handle any registered constructor updates.
-- - `scrut`: A `CCode` expression representing the value being scrutinized in the case expression ( an integer).
-- - `cases`: A list of pairs where the first element is an integer case value and the second element is the
--   corresponding `CodeBlock` representing the body for that case.
-- - `default'`: A `CodeBlock` representing the default case body.
--
-- This function returns a `CCodeGenerator` computation that produces no return value (unit type `()`). It performs
-- the following steps to generate C code for integer case expressions:
--
-- 1. **Initialize Case Handling:** It calls `initializeCaseHandling` to generate C code for checking and handling
--    any registered constructor updates.
-- 2. **Switch Statement:** It writes a C code `switch` statement using the `scrut` expression as the value to switch on.
-- 3. **Indented Block:** It uses `indented` to start an indented code block for the switch statement body.
-- 4. **Generate Cases:** It iterates over the `cases` list using `forM_` and calls `genCase` for each case.
--    The `genCase` function is explained in detail later.
-- 5. **Generate Default:** It calls `genDefault` to generate C code for the default case body.
--    The `genDefault` function is explained in detail later.
-- 6. **Close Switch:** It writes the closing curly brace `}` for the switch statement.
--
-- **Inner Function: genCase**
--
-- The `genCase` function takes a case value (`i`) and its corresponding body (`body`) as arguments. It generates
-- C code for a single case clause in the switch statement:
--
-- - It writes a C code line for the case label using `printf`.
-- - It starts an indented code block using `indented` to contain the body for this case.
-- - It calls `produceContinuationBody` to generate C code for the case body, passing the `buriedArgs` and the case
--   body (`body`). This ensures that the case body has access to the environment (buried arguments) and can execute
--   the appropriate logic for that case value.
-- - It closes the indented code block for the case clause.
--
-- **Inner Function: genDefault**
--
-- The `genDefault` function takes the default case body (`body`) as an argument. It generates C code for the
-- default clause in the switch statement:
--
-- - It writes a C code line for the default label `"default: {"`.
-- - It starts an indented code block using `indented` to contain the default body.
-- - It calls `produceContinuationBody` to generate C code for the default body, passing the `buriedArgs` and the
--   default body (`body`). This ensures that the default body has access to the environment and can execute the
--   logic for cases that don't
handleIntegerCases :: ArgumentInfo -> CCode -> CCode -> [(Int, CodeBlock)] -> CodeBlock -> CCodeGenerator ()
handleIntegerCases buriedArgs updateWith scrut cases default' = do
  initializeCaseHandling updateWith
  writeLine (printf "switch (%s) {" scrut)
  indented <| do
    forM_ cases genCase
    genDefault default'
  writeLine "}"
  where
    genCase (i, body) = do
      writeLine (printf "case %d: {" i)
      indented (produceContinuationBody buriedArgs body)
      writeLine "}"

    genDefault body = do
      writeLine "default: {"
      indented (produceContinuationBody buriedArgs body)
      writeLine "}"

-- | Generate C code for handling string case expressions.
--
-- The `handleStringCases` function takes the following arguments:
--
-- - `buriedArgs`: Information about buried arguments (environment) for the case expression.
-- - `cases`: A list of pairs where the first element is a string case value and the second element is the
--   corresponding `CodeBlock` representing the body for that case.
-- - `default'`: A `CodeBlock` representing the default case body.
--
-- This function returns a `CCodeGenerator` computation that produces no return value (unit type `()`). It performs
-- the following steps to generate C code for string case expressions:
--
-- 1. **Initialize Case Handling:** It calls `initializeCaseHandling` with a fixed update expression
--    `"update_with_string();"`. This is  because string case expressions don't involve updates in the
--    same way as constructor updates in integer case expressions.
-- 2. **Access Scrutinized String:** It writes a C code line to access the scrutinized string value. It assumes the
--    string is stored in the `g_StringRegister` global variable with an `InfoTable*` pointer at the beginning.
--    The cast `(char*)` converts the pointer to a `char*` to facilitate string comparison.
-- 3. **Generate Cases:** It iterates over the `cases` list using `forM_` and calls `genCase` for each case.
--    The `genCase` function is explained in detail later.
-- 4. **Generate Default:** It calls `genDefault` to generate C code for the default case body.
--    The `genDefault` function is explained in detail later.
--
-- **Inner Function: genCase**
--
-- The `genCase` function takes a case index (`i`) and its corresponding case value (`(s, body)`) as arguments,
-- where `s` is the string literal and `body` is the case body. It generates C code for a single case clause
-- in a series of `if`-`else if` statements:
--
-- - It determines the appropriate conditional statement type (`condType`) based on the case index (`i`).
--   - If `i == 0` (first case), it uses `"if"` to start the condition.
--   - Otherwise, it uses `"else if"` to chain subsequent conditions.
-- - It writes a C code line for the condition using `printf`. The condition checks if the scrutinized string (`scrut`)
--   is equal to the case string literal (`s`) using `strcmp`.
-- - It starts an indented code block using `indented` to contain the body for this case.
-- - It calls `produceContinuationBody` to generate C code for the case body, passing the `buriedArgs` and the case
--   body (`body`). This ensures that the case body has access to the environment and can execute the appropriate
--   logic for that case string value.
-- - It closes the indented code block for the case clause.
--
-- **Inner Function: genDefault**
--
-- The `genDefault` function takes the default case body (`body`) as an argument. It generates C code for the
-- default clause:
--
-- - It writes a closing curly brace `}` for the previous conditional block ( the last `else if`).
-- - It writes an `else` statement to start the default case block.
-- - It starts an indented code block using `indented` to contain the default body.
-- - It calls `produceContinuationBody` to generate C code for the default body, passing the `buriedArgs` and the
--   default body (`body`). This ensures that the default body has access to the environment and can execute the
--   logic for cases that don't match any of the string literals.
-- - It closes the indented code block and curly brace for the default case.
handleStringCases :: ArgumentInfo -> [(String, CodeBlock)] -> CodeBlock -> CCodeGenerator ()
handleStringCases buriedArgs cases default' = do
  initializeCaseHandling "update_with_string();"
  writeLine "char* scrut = (char*)(g_StringRegister + sizeof(InfoTable*));"
  forM_ (zip [0 ..] cases) genCase
  genDefault default'
  where
    genCase :: (Int, (String, CodeBlock)) -> CCodeGenerator ()
    genCase (i, (s, body)) = do
      let condType = if i == 0 then "if" else "} else if"
      writeLine (printf "%s (strcmp(%s, scrut) == 0) {" condType (show s))
      indented (produceContinuationBody buriedArgs body)

    genDefault body = do
      writeLine "} else {"
      indented (produceContinuationBody buriedArgs body)
      writeLine "}"

-- | Generate C code for a function body based on its type.
--
-- The `constructFunctionBody` function takes the following arguments:
--
-- - `argumentsCount`: The number of arguments the function expects.
-- - `boundArguments`: Information about bound variables (environment) for the function.
-- - `FunctionBody`: A discriminated union representing the different types of function bodies:
--     - `IntCaseBody`: Represents a case expression on an integer value.
--     - `TagCaseBody`: Represents a case expression on a constructor tag.
--     - `StringCaseBody`: Represents a case expression on a string value.
--     - `ContinuationBody`: Represents a continuation (indirect call) with an optional update type.
--     - `NormalBody`: Represents a normal function body with a sequence of instructions.
--
-- This function returns a `CCodeGenerator` computation that produces no return value (unit type `()`). It performs
-- pattern matching on the `FunctionBody` argument and delegates code generation to specific helper functions
-- based on the body type:
--
-- **Case Expressions:**
--
-- - `IntCaseBody`: If the body is an `IntCaseBody`, it calls `handleIntegerCases` to generate C code for handling
--   case expressions on integer values. This function takes care of checking for constructor updates, generating
--   a switch statement based on the scrutinized integer, and executing the appropriate case body.
-- - `TagCaseBody`: Similar to `IntCaseBody`, but for case expressions on constructor tags. It uses
--   `handleIntegerCases` with a different update expression (`"update_with_constructor();"`) to potentially
--   handle constructor updates specific to constructors.
-- - `StringCaseBody`: If the body is a `StringCaseBody`, it calls `handleStringCases` to generate C code for
--   handling case expressions on string values. This function uses a series of conditional statements (`if`-`else if`)
--   to compare the scrutinized string with each case string literal and execute the corresponding case body.
--
-- **Continuations and Normal Bodies:**
--
-- - `ContinuationBody`: If the body is a `ContinuationBody`, it determines the appropriate update expression based
--   on the `updateType` (either `IntUpdate` or `StringUpdate`). It then calls `initializeCaseHandling` to
--   check for any pending constructor updates and `produceContinuationBody` to generate C code for the continuation
--   body itself.
-- - `NormalBody`: If the body is a `NormalBody`, it calls `produceNormalFunctionBody` to generate C code for a
--   normal function body with argument processing, variable access, and instruction execution.
--
constructFunctionBody :: Int -> ArgumentInfo -> FunctionBody -> CCodeGenerator ()
constructFunctionBody argumentsCount boundArguments = \case
  IntCaseBody cases default' ->
    handleIntegerCases boundArguments "update_with_int();" "g_IntRegister" cases default'
  TagCaseBody cases default' ->
    handleIntegerCases boundArguments "update_with_constructor();" "g_TagRegister" cases default'
  StringCaseBody cases default' ->
    handleStringCases boundArguments cases default'
  ContinuationBody updateType body -> do
    let updateWith = case updateType of
          IntUpdate -> "update_with_int();"
          StringUpdate -> "update_with_string();"
    initializeCaseHandling updateWith
    produceContinuationBody boundArguments body
  NormalBody body -> produceNormalFunctionBody argumentsCount boundArguments body


-- | Generate C code for a lambda function (anonymous function).
--
-- The `genFunction` function takes a `LambdaFunction` argument representing the tinyHaskell function and returns a
-- `CCodeGenerator` computation that produces no return value (unit type `()`). This function performs the following
-- steps to generate C code for the function:
--
-- 1. **Enter Function Context:** It uses `insideFunction functionName` to enter the context of the current function,
--   setting the `functionName` for later use.
-- 2. **Function Documentation Comment:** It adds a comment to the generated C code using `comment` and `printf`
--   to document the function name.
-- 3. **Get Current Function Path:** It uses `asks currentFunction` to retrieve the current function's path
--   from the monadic state. This path is  used for generating unique names for various function-related
--   identifiers.
-- 4. **Function Information Table:** It defines several variables based on the current function path:
--     - `current`: The formatted function path using `formatFunctionPath`.
--     - `currentTable`: The name of the function's information table generated using `generateTableName`.
--     - `currentPointer`: The name of a pointer variable for the information table, generated using
--       `generateTablePointerName`.
--     - `evac`: The evacuation information variable name. This depends on the `closureType`:
--       - For `DynamicClosure`, it uses `printf "&%s"` to format the name based on the generated evacuation
--         information variable for bound arguments (`generateEvacuationInfoVariable`).
--       - For other closure types, it uses a static evacuation variable named `"static_evac"`.
-- 5. **Function Declaration:** It writes the C code for the function declaration using `printf` to format the function
--   name (`current`), return type (`void*`), and argument list (empty in this case).
-- 6. **Function Information Table Definition:** It writes the C code to define the function's information table
--   using `printf`. The information table (`currentTable`) is a struct containing:
--     - A pointer to the function itself (`&%s` formatted with `current`).
--     - A pointer to the evacuation information (`%s` using `evac`).
-- 7. **Global Closure Pointer (if applicable):** It checks the `closureType`:
--     - If `DynamicClosure`, it skips this step as there's no global pointer for dynamic closures.
--     - If `GlobalClosure _`, it writes C code to declare a global pointer variable (`currentPointer`) for the
--       information table using `printf` and assigns the address of the information table (`&%s` formatted with
--       `currentTable`).
--     - If `CAFClosure _` (CAF-based closure):
--       - It declares the global pointer variable (`currentPointer`) for the information table similar to
--         `GlobalClosure`.
--       - It defines a `CAFCell` struct (`currentCell`) using `printf` to store information about the CAF cell:
--         - A pointer to the table for CAF cells (`&table_for_caf_cell`).
--         - A pointer to the information table (`(uint8_t*)&%s` formatted with `currentPointer`).
--         - A null pointer for the continuation (filled later).
-- 8. **Generate Sub-functions:** It iterates over the `subFunctions` list (functions defined within this function)
--   and recursively calls `genFunction` for each sub-function to generate their C code.
-- 9. **Function Body:** It writes the C code for the function's opening curly brace `{` using `printf` with `current`.
-- 10. **Indented Block:** It starts an indented code block using `indented` to contain the function body code.
-- 11. **Debug Print:** It writes a C code line for debugging using `printf` to print the function name.
-- 12. **Argument Handling (Optional):** It uses `unless` to conditionally generate code for argument handling
--     based on the `argumentsCount`:
--     - If `argumentsCount > 0` (function takes arguments):
--       - It uses a `case` expression on the `closureType` to handle setting the global `g_NodeRegister` variable
--         ( for accessing arguments) based on the closure type:
--         - `GlobalClosure`: Sets `g_NodeRegister` to point to the function's InfoTable pointer (`currentPointer`).
--       - It calls `check_application_update` (assumed to be a helper function) to potentially handle application
--         updates related to the function arguments.
-- 13. **Sub-function Calls:** It calls `handleSubFunctions subFunctions` (assumed to be a helper function) to
--     generate C code for handling calls to sub-functions within the function body.
-- 14. **Apply Locations:** It calls `applyLocations maybeAllocatedClosures` (assumed to be a helper function) to
--     potentially apply locations (memory addresses) to allocated closures within the function body.
genFunction :: LambdaFunction -> CCodeGenerator ()
genFunction LambdaFunction {..} =
  insideFunction functionName <| do
    currentPath <- asks currentFunction
    let current = formatFunctionPath currentPath
        currentTable = generateTableName currentPath
        currentPointer = generateTablePointerName currentPath
        evac = case closureType of
          DynamicClosure -> printf "&%s" (generateEvacuationInfoVariable boundArguments)
          _ -> "&static_evac"
    writeLine (printf "void* %s(void);" current)
    writeLine (printf "InfoTable %s = { &%s, %s };" currentTable current evac)
    case closureType of
      DynamicClosure -> return ()
      GlobalClosure _ ->
        writeLine (printf "InfoTable* %s = &%s;" currentPointer currentTable)
      CAFClosure _ -> do
        writeLine (printf "InfoTable* %s = &%s;" currentPointer currentTable)
        let currentCell = generateCafCellIdentifier currentPath
        writeLine (printf "CAFCell %s = {&table_for_caf_cell, (uint8_t*)&%s, NULL};" currentCell currentPointer)

    forM_ subFunctions genFunction
    writeLine (printf "void* %s() {" current)
    indented <| do
      writeLine "DEBUG_PRINT(\"%s\\n\", __func__);"
      unless (argumentsCount == 0) <| do
        case closureType of
          GlobalClosure _ ->
            writeLine (printf "g_NodeRegister = (uint8_t*)&%s;" currentPointer)
          _ -> return ()
        writeLine (printf "check_application_update(%d, %s);" argumentsCount current)
      handleSubFunctions subFunctions
        <| applyLocations maybeAllocatedClosures
        <| constructFunctionBody argumentsCount boundArguments body
    writeLine "}"
  where
    maybeAllocatedClosures =
      manyLocations [(Allocated n, generateAllocatedVariable n) | n <- zipWith const [0 ..] subFunctions]

-- | Extract all literal strings used in a Cmm program.
--
-- The `gatherStrings` function takes a `Cmm` program representation as input and returns a `Set.Set String` containing
-- all unique literal strings found within the program. This function performs a recursive descent over the Cmm program
-- structure to identify and collect string literals.
--
-- **Cmm Program Structure:**
--
-- A Cmm program consists of functions (`functions`) and an entry point (`entry`).
--
-- **Function Structure:**
--
-- A function (`LambdaFunction`) is represented by various fields, including:
-- - `body`: The function body represented as a `FunctionBody`.
-- - `subFunctions`: A list of sub-functions defined within this function.
--
-- **Function Body Structure:**
--
-- A function body (`FunctionBody`) can be one of several types:
-- - `IntCaseBody`: Represents a case expression on an integer value.
-- - `StringCaseBody`: Represents a case expression on a string value.
-- - `TagCaseBody`: Represents a case expression on a constructor tag.
-- - `ContinuationBody`: Represents a continuation (indirect call) with an optional update type.
-- - `NormalBody`: Represents a normal function body with a sequence of instructions.
--
-- **Code Block Structure:**
--
-- A code block (`CodeBlock`) contains a list of instructions (`instrs`).
--
-- **Instruction Types:**
--
-- An instruction (`IRInstruction`) can be of various types relevant to string gathering:
-- - `StoreString loc`: Stores a string literal at a specific location.
-- - `BuiltinFunction1 _ loc`: Built-in function with one argument (might involve strings).
-- - `BuiltinFunction2 _ loc1 loc2`: Built-in function with two arguments (might involve strings).
-- - `BuryString loc`: Buries a string literal (makes it unavailable for direct use).
-- - `AllocString loc`: Allocates memory for a string literal.
-- - Other instructions: These might contain locations but are not relevant for string gathering.
--
-- **Location Types:**
--
-- A location (`Location`) can be of various types, but only `PrimStringLocation s` is relevant for string gathering.
--   - `PrimStringLocation s`: Represents a location containing a string literal `s`.
--
-- **Recursive Descent and String Collection:**
--
-- The `gatherStrings` function leverages a series of helper functions to recursively traverse the Cmm program structure
-- and identify string literals:
--
-- - `inFunction`: This function takes a `LambdaFunction` and calls `inFunctionBody` on its body and recursively
--   calls itself on `subFunctions` to gather strings from nested functions.
-- - `inFunctionBody`: This function takes a `FunctionBody` and dispatches to appropriate helper functions based on the
--   body type to collect strings from case branches, default bodies, or the actual code block.
-- - `inBody`: This function handles different case body types (`IntCaseBody`, `StringCaseBody`, etc.) by calling
--   `foldMap` on branches (case options) and default bodies to collect strings from code blocks within each case
--   or the default body.
-- - `inInstr`: This function takes an `IRInstruction` and checks for instruction types that might involve strings:
--   - `StoreString`: Calls `inLocation` to handle the string stored at the location.
--   - `BuiltinFunction1/2`: Calls `inLocation` on argument locations as built-in functions might operate on strings.
--   - `BuryString`, `AllocString`: Calls `inLocation` to handle the buried or allocated string.
--   - Other instructions: Returns an empty set as they don't contain strings.
-- - `inLocation`: This function checks the location type. Only `PrimStringLocation` is relevant, and it extracts
--   the string literal using `Set.singleton`.
gatherStrings :: Cmm -> Set.Set String
gatherStrings (Cmm functions entry) =
  foldMap inFunction (entry : functions)
  where
    inFunction :: LambdaFunction -> Set.Set String
    inFunction LambdaFunction {..} =
      inFunctionBody body <> foldMap inFunction subFunctions

    inFunctionBody :: FunctionBody -> Set.Set String
    inFunctionBody = \case
      IntCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      StringCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      TagCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      ContinuationBody _ body -> inBody body
      NormalBody body -> inBody body

    inBody :: CodeBlock -> Set.Set String
    inBody (CodeBlock _ _ instrs) = foldMap inInstr instrs

    inInstr :: IRInstruction -> Set.Set String
    inInstr = \case
      StoreString loc -> inLocation loc
      BuiltinFunction2 _ loc1 loc2 -> inLocation loc1 <> inLocation loc2
      BuiltinFunction1 _ loc -> inLocation loc
      BuryString loc -> inLocation loc
      AllocString loc -> inLocation loc
      -- Some of these contain locations, but shouldn't contain strings
      _ -> mempty

    inLocation :: Location -> Set.Set String
    inLocation (PrimStringLocation s) = Set.singleton s
    inLocation _ = mempty

-- | Generate C code for static strings used in the program.
--
-- The `genStaticStrings` function takes a `Set.Set String` containing all unique literal strings from the program and
-- returns a `CCodeGenerator CCodeLocationMap` computation. This function performs the following steps:
--
-- 1. **Indexed Strings:** It converts the set of strings (`strings`) into a list of pairs `[(SubFunctionIndex, String)]`
--   using `Set.toList` and `zip`. Here:
--     - `SubFunctionIndex`: Represents an index for sub-functions within a function ( used for unique naming).
--     - `String`: The actual string literal.
-- 2. **Generate String Literal Variable:** It iterates over the list of indexed strings using `foldMapM`. Inside the loop,
--   it calls `makeLocation` for each string, providing the index (`i`) and the string literal (`s`).
--
-- **Inner Function: makeLocation**
--
-- The `makeLocation` function takes a `SubFunctionIndex` (`i`) and a `String` (`s`) as arguments and returns a
-- `CCodeGenerator CCodeLocationMap` computation. It performs the following steps to generate C code for a single
-- static string:
--
-- - **String Information Structure:** It defines a C structure to hold information about the static string:
--   - `InfoTable* table`: A pointer to a dummy information table ( not used for static strings).
--   - `char data[bytes]`: An array of characters to store the string data. The size (`bytes`) is calculated
--     using `length s + 1` to include the null terminator.
-- - **Static String Variable:** It defines a static string variable name (`var`) using `generateStringLiteralVariable i`.
-- - **C Code for String Variable Definition:** It writes C code for the static string variable definition using
--   indentation and `printf`:
--     - The opening curly brace `{` for the structure.
--     - The indented code block containing:
--       - `InfoTable* table;` (placeholder)
--       - `char data[%d];` (array declaration with calculated size).
--     - The closing curly brace `}` and variable assignment using `printf`:
--       - `%s = { &table_for_string_literal, %s };` (formats the variable name, assigns the structure with
--         a dummy information table pointer and the string data).
-- - **Location Mapping:** It calls `createSingleLocation` to create a mapping between the string literal location
--   (`PrimStringLocation s`) and its corresponding C code location in memory (`"(uint8_t*)&" <> var`). This
--   mapping is  used later for code generation involving string literals.
-- - **Return:** The function returns the updated `CCodeLocationMap` with the newly created mapping.
genStaticStrings :: Set.Set String -> CCodeGenerator CCodeLocationMap
genStaticStrings strings =
  foldMapM (uncurry makeLocation) indexedStrings
  where
    indexedStrings :: [(SubFunctionIndex, String)]
    indexedStrings = strings |> Set.toList |> zip [0 ..]

    makeLocation :: SubFunctionIndex -> String -> CCodeGenerator CCodeLocationMap
    makeLocation i s = do
      let bytes = length s + 1
          var = generateStringLiteralVariable i
      writeLine "struct {"
      indented <| do
        writeLine "InfoTable* table;"
        writeLine (printf "char data[%d];" bytes)
      writeLine (printf "} %s = { &table_for_string_literal, %s };" var (show s))
      return (createSingleLocation (PrimStringLocation s) ("(uint8_t*)&" <> var))

-- | Extract all bound argument shapes (types and sizes) used in a Cmm program.
--
-- The `gatherBoundArgTypes` function takes a `Cmm` program representation as input and returns a `Set.Set ArgumentInfo`
-- containing all unique bound argument shapes found within the program. This function performs a recursive descent over
-- the Cmm program structure to identify and collect bound argument information.
--
-- **Bound Argument Shapes:**
--
-- A bound argument shape is represented by the `ArgumentInfo` type, which  contains information about:
-- - Argument types
-- - Argument sizes
--
-- **Cmm Program Structure:**
--
-- A Cmm program consists of functions (`functions`) and an entry point (`function`).
--
-- **Function Structure:**
--
-- A function (`LambdaFunction`) is represented by various fields, including:
-- - `closureType`: The type of closure the function uses (e.g., `DynamicClosure`).
-- - `boundArguments`: Information about the function's bound arguments ( types and sizes).
-- - `subFunctions`: A list of sub-functions defined within this function.
--
-- **Recursive Descent and Argument Shape Collection:**
--
-- The `gatherBoundArgTypes` function leverages a helper function `inFunction` to traverse the Cmm program structure
-- and collect bound argument shapes:
--
-- - `inFunction`: This function takes a `LambdaFunction` and calls itself recursively on `subFunctions` to gather shapes
--   from nested functions. It also checks the `closureType`:
--     - If `DynamicClosure`, it adds the function's `boundArguments` to the set (potentially GC-ed).
--     - For other closure types, it returns an empty set as they don't have dynamic arguments to be collected.
gatherBoundArgTypes :: Cmm -> Set.Set ArgumentInfo
gatherBoundArgTypes (Cmm functions function) =
  foldMap inFunction (function : functions)
  where
    inFunction :: LambdaFunction -> Set.Set ArgumentInfo
    inFunction LambdaFunction {..} =
      inThisFunction <> foldMap inFunction subFunctions
      where
        -- We only include this pattern if it can be potentially GCed
        inThisFunction = case closureType of
          DynamicClosure -> Set.singleton boundArguments
          _ -> Set.empty

-- | Generate C code for an evacuation function for a specific argument shape.
--
-- The `genEvacFunction` function takes an `ArgumentInfo` argument representing the bound argument shape (types and sizes)
-- of a function and returns a `CCodeGenerator ()` computation that produces no return value (unit type `()`). This
-- function generates C code for an evacuation function that relocates closures and their bound arguments during garbage
-- collection.
--
-- **Argument Shape:**
--
-- The `ArgumentInfo` type  contains information about:
-- - `boundPointers`: The number of bound pointer arguments.
-- - `boundStrings`: The number of bound string arguments.
-- - `boundInts`: The number of bound integer arguments (might include size information).
--
-- **Evacuation Function:**
--
-- The generated evacuation function has the following structure:
--
-- - **Function Signature:**
--   - `uint8_t* %s(uint8_t* base)`:
--     - The function name is generated using `generateEvacuationInfoVariable` based on the argument shape (`info`).
--     - It takes a `uint8_t* base` argument, which points to the memory location of the closure to be evacuated.
--     - It returns a `uint8_t*`, which points to the new location of the evacuated closure.
-- - **Function Body:**
--   - **Relocating Closure:**
--     - The function calculates the total size (`closure_size`) required for the evacuated closure, considering:
--       - Size of the information table pointer (`sizeof(InfoTable*)`).
--       - Size of each bound pointer and string (based on `boundPointers` and `boundStrings`) using multiplication
--         with the corresponding size (`sizeof(uint8_t*)`).
--       - Size of each bound integer (based on `boundInts`) using multiplication with the corresponding size
--         (`sizeof(int64_t)`).
--     - It allocates memory for the evacuated closure on the heap using `heap_cursor()` and `heap_write(base, closure_size)`.
--   - **Replacing Old Closure with Indirection:**
--     - The function replaces the original closure pointer at `base` with an indirection to the new location. This is done
--       by copying:
--         - The address of a pre-defined "already evacuated" information table pointer (`table_pointer_for_already_evac`)
--           using `memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable*))`.
--         - The address of the newly allocated memory (`new_base`) using `memcpy(base + sizeof(InfoTable*), &new_base, sizeof(uint8_t*))`.
--   - **Evacuating Roots Recursively:**
--     - It iterates over bound pointers:
--       - Reads the pointer value from the current location using `read_ptr(cursor)`.
--       - Calls `collect_root(&root)` (assumed to be a garbage collection helper function) to mark the pointed-to
--         data as a root for garbage collection.
--       - Copies the evacuated pointer value back to the new location using `memcpy(cursor, &root, sizeof(uint8_t*))`.
--       - Increments the cursor (`cursor += sizeof(uint8_t*)`) to point to the next bound pointer location.
--     - If there are bound integers (checked with `unless (boundInts == 0)`):
--       - It skips over the integer data area in the evacuated closure by incrementing the cursor
--         appropriately (`cursor += boundInts * sizeof(int64_t)`).
--     - It iterates over bound strings (similarly to bound pointers):
--       - Reads the pointer value, calls `collect_root`, copies the evacuated pointer, and increments the cursor.
--   - **Return Value:**
--     - The function returns the `new_base` pointer, which points to the evacuated closure's new location.
genEvacFunction :: ArgumentInfo -> CCodeGenerator ()
genEvacFunction info@ArgumentInfo {..} = do
  let thisFunction = generateEvacuationInfoVariable info
  writeLine (printf "uint8_t* %s(uint8_t* base) {" thisFunction)
  indented <| do
    writeLine "size_t closure_size = sizeof(InfoTable*);"
    case info of
      ArgumentInfo 0 0 0 ->
        writeLine "closure_size += sizeof(uint8_t*);"
      _ -> do
        writeLine
          ( printf
              "closure_size += %d * sizeof(uint8_t*);"
              (boundPointers + boundStrings)
          )
        writeLine (printf "closure_size += %d * sizeof(int64_t);" boundInts)
    writeLine "uint8_t* new_base = heap_cursor();"
    writeLine "heap_write(base, closure_size);"
    writeLine "memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable*));"
    writeLine "memcpy(base + sizeof(InfoTable*), &new_base, sizeof(uint8_t*));"
    writeLine "uint8_t* cursor = new_base + sizeof(InfoTable*);"
    writeLine "uint8_t* root;"
    replicateM_ boundPointers <| do
      writeLine "root = read_ptr(cursor);"
      writeLine "collect_root(&root);"
      writeLine "memcpy(cursor, &root, sizeof(uint8_t*));"
      writeLine "cursor += sizeof(uint8_t*);"
    unless (boundInts == 0)
      <| writeLine (printf "cursor += %d * sizeof(int64_t);" boundInts)
    replicateM_ boundStrings <| do
      writeLine "root = read_ptr(cursor);"
      writeLine "collect_root(&root);"
      writeLine "memcpy(cursor, &root, sizeof(uint8_t*));"
      writeLine "cursor += sizeof(uint8_t*);"
    writeLine "return new_base;"
  writeLine "}"
  writeLine ""

-- | Generate C code for the `main` function.
--
-- The `genMainFunction` function is a `CCodeGenerator ()` computation that produces no return value (unit type `()`).
-- This function generates the main entry point of the C program translated from tinyHaskell code.
--
-- **C Code Structure:**
--
-- The generated `main` function has the following structure:
--
-- - **Function Signature:**
--   - `int main()`: The standard `main` function signature for C programs. It returns an integer value (`int`).
-- - **Function Body:**
--   - **Setup:**
--     - It calls a function named `setup()`. This function is  responsible for any initialization tasks required
--       before running the tinyHaskell program (e.g., allocating memory, initializing modules).
--   - **Entry Point Loop:**
--     - It retrieves the entry point of the tinyHaskell program using `formatFunctionPath (FunctionPath [Entry])`. The
--       `FunctionPath` constructor with a single element `Entry`  represents the starting point of the program.
--     - It defines a `CodeLabel` variable named `label` and assigns the address of the entry point function using
--       `printf("CodeLabel label = &%s;", entry)`. The `CodeLabel` type  represents a pointer to a function
--       representing a code block in the translated program.
--     - It enters a loop that continues as long as the `label` is not `NULL`:
--       - Inside the loop, the current code block is executed by calling the function pointed to by `label` and
--         casting the result to `CodeLabel` using `label = (CodeLabel)label();`. This assumes that each code block
--         returns a pointer to the next code block to execute, or `NULL` if it's the end.
--   - **Cleanup:**
--     - After the loop exits (when `label` becomes `NULL`), it calls a function named `cleanup()`. This function is
--        responsible for any necessary cleanup tasks before program termination (e.g., deallocating memory).
--   - **Return:**
--     - The function returns 0 to indicate successful program termination.
--
-- The `genMainFunction` plays a vital role in setting up the execution flow for the translated C program. It
-- initializes the environment, calls the tinyHaskell program's entry point, and handles cleanup after program execution.
-- The `CodeLabel` type and the way code blocks are chained together suggest a continuation-based approach
-- for representing and executing the translated tinyHaskell code.
genMainFunction :: CCodeGenerator ()
genMainFunction = do
  writeLine "int main() {"
  indented <| do
    writeLine "setup();"
    let entry = formatFunctionPath (FunctionPath [Entry])
    writeLine (printf "CodeLabel label = &%s;" entry)
    writeLine "while (label != NULL) {"
    indented (writeLine "label = (CodeLabel)label();")
    writeLine "}"
    writeLine "cleanup();"
    writeLine "return 0;"
  writeLine "}"


-- | Generate C code for the Cmm intermediate representation (IR).
--
-- The `genCmm` function takes a `Cmm` program representation as input and performs various C code generation tasks
-- using the `CCodeGenerator` monad. It returns a `CCodeGenerator ()` computation that produces no return value (unit type `()`).
--
-- **Cmm Program Representation:**
--
-- A `Cmm` program consists of functions (`functions`) and an entry point (`entry`).
--
-- **C Code Generation Steps:**
--
-- 1. **Include Header:**
--   - It writes an `#include` directive for the `garbage-collection.c` header file, assuming it contains necessary
--     functions for garbage collection.
-- 2. **Generate Static Strings:**
--   - It calls `gatherStrings cmm` to collect all unique literal strings used in the program.
--   - It calls `genStaticStrings` with the collected strings to generate C code for static string definitions.
--     - The result of `genStaticStrings` is bound to `stringLocations`, which  contains information about the
--       memory locations of the generated static strings.
-- 3. **Generate Evacuation Functions:**
--   - It calls `gatherBoundArgTypes cmm` to collect all unique bound argument shapes (types and sizes) found in the
--     program.
--   - It iterates over the collected argument shapes using `forM_` and calls `genEvacFunction` for each shape.
--     - This generates C code for evacuation functions responsible for relocating closures and their bound arguments
--       during garbage collection.
-- 4. **Apply String Locations (Optional):**
--   - It calls `applyLocations stringLocations` (assuming this function applies the information from `stringLocations`
--     to previously generated C code, potentially patching locations of static strings).
-- 5. **Generate C Code for Functions:**
--   - It iterates over the program functions (`functions`) using `forM_` and calls `genFunction` for each function.
--     - This generates C code for each function defined in the program.
--   - It calls `genFunction entry` to generate C code for the program's entry point.
-- 6. **Generate Main Function:**
--   - It calls `genMainFunction` to generate C code for the `main` function, which sets up the environment,
--     calls the program's entry point, and handles cleanup.
--
-- The `genCmm` function is a central part of the C code generation process in tinyGHC. It takes the Cmm IR and
-- generates various C code elements like static string definitions, evacuation functions, function bodies, and the
-- `main` function. It leverages other helper functions like `genStaticStrings`, `genEvacFunction`, and `genMainFunction`
-- to achieve this.
genCmm :: Cmm -> CCodeGenerator ()
genCmm cmm@(Cmm functions entry) = do
  writeLine "#include \"garbage-collection.c\"\n"
  comment "This C code is generated by tinyGHC."
  stringLocations <- genStaticStrings (gatherStrings cmm)
  writeLine ""
  forM_ (gatherBoundArgTypes cmm) genEvacFunction
  writeLine ""
  applyLocations stringLocations <| do
    forM_ functions <| \f -> do
      genFunction f
      writeLine ""
    genFunction entry
    writeLine ""
    writeLine ""
    genMainFunction

-- | Convert our Cmm IR into actual C code
generateCCode :: Cmm -> CCode
generateCCode cmm =
  let ((globals, cafs), _) = runCWriter (collectGlobalFunctions cmm)
   in genCmm cmm |> withGlobals globals |> processCafs cafs |> runCWriter |> snd
