{-# LANGUAGE TypeApplications #-}

{- |
Module      : tinyGHC
Description : Command Line Interface for the tinyGHC compiler.
Copyright   : (c) Amr Almorsi, 2024
License     : BSD3
Maintainer : amr.saleh@ejust.edu.eg
Stability   : experimental
Portability : POSIX

This module provides the command line interface for the tinyGHC compiler,
which translates tinyHaskell code into C code. It defines stages of the
transpilation process, handles command line arguments, and executes
the appropriate stages based on user input.
-}

module Main where

    
import qualified CCodeGenerator
import qualified Cmm
import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import qualified Lexer
import Ourlude
import qualified Parser
import qualified STG
import qualified ASTSimplifier
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrint, pPrintString)
import qualified TypeChecker
import Types (PolyType)


-- | Represents an error that occurs during a compilation stage.
--
-- The `StagedError` data type encapsulates information about an error
-- encountered during a specific stage of the compilation process. It
-- stores two fields:
--
-- * `stageName`: A `String` containing the name of the stage where the error
--   occurred.
-- * `errorMessage`: A `String` containing the error message describing
--   the issue encountered.
--
-- This data type is used to provide context and detailed information
-- about compilation errors.
data StagedError = StagedError String String

-- | Wraps a result or error with a stage name for better error reporting.
--
-- The `stageEither` function takes a `String` representing the stage name
-- and an `Either` value containing either a result (`Right`) or an error
-- (`Left`). It performs the following actions:
--
-- * If the `Either` value is `Left` (error), it creates a `StagedError`
--   instance with the provided stage name and the error message extracted
--   using `show e`. This associates the error with the specific stage
--   where it occurred.
-- * If the `Either` value is `Right` (successful result), it simply
--   returns the result without modification.
--
-- This function is used throughout the compilation pipeline to wrap
-- stage results and errors with the corresponding stage name for improved
-- error reporting. It provides context for error messages and helps
-- identify which stage caused the issue.
stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name m = case m of
  Left e -> Left (StagedError name (show e))
  Right a -> Right a


printStagedError :: StagedError -> IO ()
printStagedError (StagedError name err) = do
  putStrLn (name ++ " Error:")
  pPrintString err

-- | Represents a stage in the compilation pipeline.
--
-- The `Stage` data type defines a unit of processing within the tinyGHC
-- compiler. It encapsulates two key components:
--
-- * `name`: A `String` representing the name of the stage. This name
--   identifies the specific processing step performed by the stage.
-- * `runStage`: A function that takes input of type `a` and attempts to
--   process it. This function returns an `Either StagedError b` value.
--     * `Right b`: Successful processing, returning the stage's output of type `b`.
--     * `Left (StagedError name err)`: Error encountered during processing.
--       - `name`: The stage name (obtained from the `Stage` itself).
--       - `err`: A `String` containing the error message describing the issue.
--
-- Stages are the building blocks of the Tiny tinyGHC compiler. The compilation
-- process is defined as a sequence of stages, each performing a specific
-- task on the code (e.g., lexical analysis, parsing, type checking).
data Stage a b = Stage
  { name :: String,
    runStage :: a -> Either StagedError b
  }


-- | Represents a stage in the compilation pipeline.
--
-- (Refer to the previous explanation for `Stage` data type)

-- | Creates a compilation stage.
--
-- The `makeStage` function constructs a new `Stage` value given a stage name
-- and a computation function. It performs the following actions:
--
-- * `name`: The provided `String` argument is used as the stage name.
-- * `r`: The given function (`(a -> Either e b)`) represents the computation
--   performed by the stage. It takes input of type `a` and attempts to process
--   it, returning an `Either e b` value.
--     * `Right b`: Successful processing, returning the stage's output of type `b`.
--     * `Left err`: Error encountered during processing (type `e` depends on
--       the specific computation).
--
-- However, tinyGHC utilizes `StagedError` for consistent error handling
-- across stages. Therefore, `makeStage` applies the `stageEither` function
-- (using `>>>` for monad composition) to wrap the result or error from `r`
-- with the stage name in a `StagedError`. This ensures all stage errors
-- have context (stage name) for better reporting.
makeStage :: Show e => String -> (a -> Either e b) -> Stage a b
makeStage name r = Stage name (r >>> stageEither name)

-- | Composes two compilation stages.
--
-- The `(>->)` operator (function) combines two `Stage` values into a new
-- composite stage. It performs the following actions:
--
-- * Takes two `Stage` arguments: `(Stage _ r1)` and `(Stage n2 r2)`.
--   - The first stage (`r1`) has an unknown name (`_`) as it's not used
--     in the composition.
--   - The second stage (`n2`) provides the name for the resulting composite
--     stage.
-- * The `r1 >=> r2` expression composes the stage computations using monad
--   composition (`(>=>)`) from the `Either` monad. This ensures proper error
--   handling propagation through the pipeline.
--
-- The resulting composite stage executes the first stage (`r1`) followed by
-- the second stage (`r2`) on the input data. Any errors encountered during
-- either stage are propagated through the `Either` monad.
(>->) :: Stage a b -> Stage b c -> Stage a c
(>->) (Stage _ r1) (Stage n2 r2) = Stage n2 (r1 >=> r2)


-- | Executes a compilation stage and handles the result or error.
--
-- The `printStage` function takes a `Stage` value and its corresponding input data.
-- It performs the following actions:
--
-- * `stage`: The `Stage` argument encapsulates the stage to be executed.
--   It provides the stage name (`name`) and the computation function (`runStage`).
-- * `input`: The input data of type `a` to be processed by the stage.
--
-- The function uses a case expression on the result of running the stage's
-- computation (`runStage input`).
--
-- * `Left err`: If an error (`err`) occurs during stage execution, it performs
--   the following:
--     * Calls `printStagedError` to print a formatted error message including
--       the stage name and the error details.
--     * Calls `exitFailure` to terminate the program with an error code.
-- * `Right b`: If the stage execution is successful, it performs the following:
--     * Prints the stage name followed by a colon (`:`).
--     * Uses `pPrint b` to pretty-print the stage's output (`b`) of type `b`.
printStage :: Show b => Stage a b -> a -> IO ()
printStage (Stage name r) a = case r a of
  Left err -> do
    printStagedError err
    exitFailure
  Right b -> do
    putStrLn (name ++ ":")
    pPrint b

-- | Defines the lexical analysis stage.
--
-- The `lexerStage` value represents the lexical analysis stage within the Tiny tinyGHC
-- compiler pipeline. It is created using the `makeStage` function, providing:
--
-- * Stage name: "Lexer" (set in `makeStage`).
-- * Stage computation: `Lexer.lexer` (the lexer function from the `Lexer` module).
--
-- The `Lexer.lexer` function takes the tinyHaskell source code as a string and attempts
-- to tokenize it. If successful, it returns a list of `Lexer.Token` values
-- representing the identified tokens (keywords, identifiers, literals, etc.).
-- On error, an `Either StagedError [Lexer.Token]` value is returned with the error
-- details encapsulated in a `StagedError`.
--
-- This stage is the first step in the compilation process, responsible for
-- breaking down the source code into its basic lexical units.
lexerStage :: Stage String [Lexer.Token]
lexerStage = makeStage "Lexer" Lexer.lexer

-- | Defines the parsing stage.
--
-- The `parserStage` value represents the parsing stage within the Tiny tinyGHC compiler
-- pipeline. It is created using the `makeStage` function, providing:
--
-- * Stage name: "Parser" (set in `makeStage`).
-- * Stage computation: `Parser.parser` (the parser function from the `Parser` module).
--
-- The `Parser.parser` function takes the list of tokens produced by the lexer stage
-- (`[Lexer.Token]`) and attempts to parse them according to tinyHaskell grammar. If
-- successful, it returns an abstract syntax tree (AST) representing the parsed
-- program structure using the `Parser.AST` type. On error, an
-- `Either StagedError Parser.AST` value is returned with the error details
-- encapsulated in a `StagedError`.
--
-- This stage follows the lexical analysis stage and builds a hierarchical structure
-- representing the program's syntax based on the identified tokens.
parserStage :: Stage [Lexer.Token] Parser.AST
parserStage = makeStage "Parser" Parser.parser

-- | Defines the simplification stage.
--
-- The `simplifierStage` value represents the simplification stage within the Tiny tinyGHC
-- compiler pipeline. It is created using the `makeStage` function, providing:
--
-- * Stage name: "ASTSimplifier" (set in `makeStage`).
-- * Stage computation: `ASTSimplifier.simplifier` (the simplifier function from the
--   `ASTSimplifier` module).
--
-- The `ASTSimplifier.simplifier` function takes the AST generated by the parsing stage
-- (`Parser.AST`) and attempts to simplify it. This might involve optimizations,
-- constant folding, or other transformations to improve the code's efficiency
-- while preserving its semantics. On error, an `Either StagedError (ASTSimplifier.AST ())`
-- value is returned with the error details encapsulated in a `StagedError`.
--
-- This stage is optional and may not be included in all compilation pipelines. It
-- can be used to optimize the code before further processing.
simplifierStage :: Stage Parser.AST (ASTSimplifier.AST ())
simplifierStage = makeStage "ASTSimplifier" ASTSimplifier.simplifier

-- | Defines the type checking stage.
--
-- The `typeCheckerStage` value represents the type checking stage within the Tiny tinyGHC compiler
-- pipeline. It is created using the `makeStage` function, providing:
--
-- * Stage name: "TypeChecker" (set in `makeStage`).
-- * Stage computation: `TypeChecker.typer` (the type checker function from the `TypeChecker` module).
--
-- The `TypeChecker.typer` function takes the (potentially) simplified AST generated by the
-- previous stage (`ASTSimplifier.AST ()`) and attempts to assign types to the program's
-- expressions and variables. If successful, it returns a new AST annotated with type
-- information using the `ASTSimplifier.AST PolyType` type. On error, an
-- `Either StagedError (ASTSimplifier.AST PolyType)` value is returned with the error details
-- encapsulated in a `StagedError`.
--
-- This stage ensures the program is well-typed, meaning each expression and variable
-- has a valid type according to the tinyHaskell language rules. Type checking helps catch
-- potential errors early in the compilation process.
typeCheckerStage :: Stage (ASTSimplifier.AST ()) (ASTSimplifier.AST PolyType)
typeCheckerStage = makeStage "TypeChecker" TypeChecker.typer

-- | Defines the single static assignment (SSA) generation stage.
--
-- The `stgStage` value represents the stage for generating Single Static Assignment
-- (SSA) form within the Tiny tinyGHC compiler pipeline. It is created using the
-- `makeStage` function, providing:
--
-- * Stage name: "STG" (set in `makeStage`).
-- * Stage computation: `STG.compileToSTG` (the SSA generation function from the `STG` module).
--
-- The `STG.compileToSTG` function takes the type-annotated AST (`ASTSimplifier.AST PolyType`) and
-- attempts to convert it into SSA form. In SSA, each variable is assigned a value
-- only once, simplifying analysis and optimization. On error, an
-- `Either StagedError STG.STG` value is returned with the error details encapsulated
-- in a `StagedError`.
--
-- This stage optimizes the program's intermediate representation for further processing.
stgStage :: Stage (ASTSimplifier.AST PolyType) STG.STG
stgStage = makeStage "STG" STG.compileToSTG

-- | Defines the Cmm (C-like intermediate representation) generation stage.
--
-- The `cmmStage` value represents the stage for generating Cmm code within the Tiny tinyGHC
-- compiler pipeline. Cmm is a C-like intermediate representation used for further
-- optimization and code generation. It is created using the `makeStage` function,
-- providing:
--
-- * Stage name: "Cmm" (set in `makeStage`).
-- * Stage computation: `Cmm.cmm >>> Right @()` (a composition of functions).
--   - `Cmm.cmm`: The function from the `Cmm` module responsible for generating Cmm code
--     from the SSA form (`STG.STG`). On error, it might return an `Either` value with
--     the error details.
--   - `Right @()`: This ensures the `cmmStage` always returns a `Right` value even
--     if `Cmm.cmm` doesn't produce an output (indicated by `()`). This simplifies error
--     handling in the pipeline.
--
-- The `Cmm.cmm` function takes the Spinless Tagless G-machine representation (`STG`) produced by the STG
-- generation stage and attempts to convert it into Cmm code. Cmm is a C-like intermediate
-- representation that is easier to translate into real C code compared to the original
-- tinyHaskell AST. On error, an `Either StagedError Cmm.Cmm` value is returned with the error
-- details encapsulated in a `StagedError`.
--
-- This stage translates the program into a lower-level representation closer to the target
cmmStage :: Stage STG.STG Cmm.Cmm
cmmStage = makeStage "Cmm" (Cmm.cmm >>> Right @())


-- | Defines the C code generation stage.
--
-- The `generateCCodeStage` value represents the C code generation stage within the Tiny tinyGHC compiler
-- pipeline. It is created using the `makeStage` function, providing:
--
-- * Stage name: "Output C" (set in `makeStage`).
-- * Stage computation: `CCodeGenerator.writeC >>> Right @()` (a composition using `>>>`).
--   - `CCodeGenerator.writeC`: The C code generation function from the `CCodeGenerator` module.
--   - `Right @()`: Wraps the generation result (a `String` containing C code) in `Right` using
--     the identity monad (`@()`) to ensure a consistent `Either StagedError String` return type.
--
-- The `CCodeGenerator.writeC` function takes the Cmm program (`Cmm.Cmm`) and attempts to generate
-- equivalent C code as a string. This is the final stage, translating the high-level tinyHaskell
-- program into a compilable C representation. On successful generation, a `String` containing
-- the C code is returned. On error, an `Either StagedError String` value is returned with the
-- error details encapsulated in a `StagedError`.
generateCCodeStage :: Stage Cmm.Cmm String
generateCCodeStage = makeStage "Output C" (CCodeGenerator.generateCCode >>> Right @())

-- | Parses a string representing the compilation stage(s) to execute.
--
-- The `readStage` function takes a `String` argument representing the desired stage(s)
-- to be executed and an optional `Maybe String` argument potentially containing the
-- output file path (used in the "compile" stage). It returns a `Maybe (String -> IO ())` value.
--
-- * `Maybe (String -> IO ())`: This type represents an optional computation that takes
--   a string argument and returns an `IO ()` value (performs some side effects).
--   - `Just (stagePipeline)`: If the stage string is recognized and parsed successfully,
--     the function returns `Just` containing a composite stage pipeline (`stagePipeline`)
--     built using `(>->)`. This pipeline executes the requested stages and potentially prints
--     the results using `printStage`.
--   - `Nothing`: If the stage string is invalid or there's an error during parsing,
--     the function returns `Nothing`.
--
-- The function definition handles various stage names:
--
-- * `"lex"`: Executes only the lexer stage (`lexerStage`) and prints the results.
-- * `"parse"`: Executes the lexer stage (`lexerStage`) followed by the parser stage
--   (`parserStage`) and prints the results.
-- * `"simplify"`: Executes the lexer stage (`lexerStage`), parser stage (`parserStage`),
--   and simplifier stage (`simplifierStage`) and prints the results.
-- * `"type"`: Executes the entire pipeline: lexer (`lexerStage`), parser (`parserStage`),
--   simplifier (`simplifierStage`), and type checker (`typeCheckerStage`) stages, printing the
--   results at each stage.
-- * `"compileToSTG"`: Similar to `"type"`, executes the full pipeline including single static
--   assignment (SSA) generation (`stgStage`).
-- * `"cmm"`: Similar to `"compileToSTG"`, executes the full pipeline including Cmm intermediate
--   representation (IR) generation (`cmmStage`).
-- * `"compile"`: Executes the full pipeline and generates C code using `generateCCodeStage`.
--   If an output file path is provided in the `Maybe String` argument, it writes the
--   generated C code to the file using `outputStage`.
--
-- The `outputStage` function is a helper function used only in the `"compile"` stage.
-- It takes a `Stage` value and its input data. It attempts to execute the stage and
-- handles the result:
--   - On error (`Left err`), it prints the staged error using `printStagedError`.
--   - On success (`Right output`), it writes the output (C code string) to the specified
--     output file using `writeFile`.
readStage :: String -> Maybe String -> Maybe (String -> IO ())
readStage stageName maybeOutputFile = case stageName of
 "tokens" -> Just (lexerStage |> printStage)
 "ast" -> Just (lexerStage >-> parserStage |> printStage)
 "simplify-ast" -> Just (lexerStage >-> parserStage >-> simplifierStage |> printStage)
 "type-check" -> Just (lexerStage >-> parserStage >-> simplifierStage >-> typeCheckerStage |> printStage)
 "compileToSTG-ir" -> Just (lexerStage >-> parserStage >-> simplifierStage >-> typeCheckerStage >-> stgStage |> printStage)
 "cmm-ir" -> Just (lexerStage >-> parserStage >-> simplifierStage >-> typeCheckerStage >-> stgStage >-> cmmStage |> printStage)
 "ccode" -> do
    outputFile <- maybeOutputFile
    let outputStage (Stage _ r) a = case r a of
          Left err -> printStagedError err
          Right output -> writeFile outputFile output
    return $ lexerStage >-> parserStage >-> simplifierStage >-> typeCheckerStage >-> stgStage >-> cmmStage >-> generateCCodeStage |> outputStage
readStage _ _ = Nothing




-- | Command-line argument data type.
--
-- The `Args` data type defines the structure of the arguments accepted by the Tiny tinyGHC
-- compiler. It holds two values:
--
-- * `filePath`: A `FilePath` representing the path to the tinyHaskell source code file to be
--   transpiled.
-- * `stagePipeline`: A function of type `String -> IO ()` representing the stage pipeline
--   to be executed. This function is built based on the requested compilation stage(s)
--   provided by the user.
data Args = Args FilePath (String -> IO ())

-- | Parses command-line arguments.
--
-- The `parseArgs` function takes a list of `String` arguments provided on the command line
-- and attempts to parse them into an `Args` value. It returns a `Maybe Args` value:
--
-- * `Just Args`: If the arguments are valid and parsed successfully, the function returns
--   `Just` containing an `Args` value with the parsed file path and stage pipeline.
-- * `Nothing`: If there's an error during parsing (e.g., invalid stage name or missing file path),
--   the function returns `Nothing`.
--
-- The function logic checks the number of arguments:
--
-- * If there are less than two arguments, it indicates missing information and returns `Nothing`.
-- * Otherwise, it extracts the stage name (converted to lowercase using `map toLower`),
--   the optional output file path (using `listToMaybe`), and attempts to build the stage
--   pipeline using `readStage`. If successful, it returns `Just Args` with the parsed data.
parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : rest) = do
  let outputFile = listToMaybe rest
  stage <- readStage (map toLower stageName) outputFile
  return (Args file stage)
parseArgs _ = Nothing

-- | Processes the parsed arguments and executes the compilation pipeline.
--
-- The `process` function takes an `Args` value containing the parsed file path and stage
-- pipeline. It performs the following actions:
--
-- * Reads the content of the tinyHaskell source code file using `readFile`.
-- * Executes the stage pipeline (`stage`) on the read content, potentially performing
--   lexing, parsing, simplification, type checking, and C code generation based on the
--   requested stages.
process :: Args -> IO ()
process (Args path stage) = do
  content <- readFile path
  stage content

-- | Main entry point for the Tiny tinyGHC compiler.
--
-- The `main` function is the entry point of the program. It performs the following actions:
--
-- * Retrieves the command-line arguments as a list of strings using `getArgs`.
-- * Attempts to parse the arguments using `parseArgs`.
--   - If parsing fails (`Nothing`), it prints an error message indicating invalid arguments.
--   - If parsing succeeds (`Just arguments`), it calls `process` with the parsed arguments
--     to perform the actual compilation based on the requested stage(s).
main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized options"
    Just arguments -> process arguments

