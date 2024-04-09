{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | The Lexer module: Tokenizing tinyHaskell code

module Lexer (Token (..), lexer) where


import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe, maybeToList)
import Ourlude


-- | Represents the kind of error that can occur during the lexing phase of tinyGHC.
data LexingError
  = -- We encountered a character that we weren't expecting according to the tinyHaskell grammar.
    UnexpectedChar Char
  | -- We reached the end of the input string (`EOF`) while still expecting characters to lex.
    UnexpectedEOF
  | -- An indentation layout block was opened with a `{` but not properly closed with a `}`.
    UnmatchedLayout
  deriving (Show, Eq)

-- | Creates a `LexingError` to indicate encountering an unexpected character during lexing.
unexpected :: String -> LexingError
unexpected [] = UnexpectedEOF
unexpected (c : _) = UnexpectedChar c
-- | **Connection to EBNF Grammar:** This `LexingError` type captures potential mismatches between the input characters encountered
-- during lexing and the characters expected based on the tinyHaskell grammar defined using Extended Backus-Naur Form (EBNF).
-- The `UnexpectedChar` variant indicates a character that does not fit any expected pattern in the grammar.


-- | The `Lexer` monad type encapsulates the lexing process in tinyGHC.
-- It takes an input string as its argument and returns an `Either` value.
-- The `Either` value can be either a `Left` containing a `LexingError`
-- that indicates a lexing error, or a `Right` containing a pair of the lexed value (`a`)
-- and the remaining unlexed input string.
--
-- `Lexer` functions like parser combinators, but with some key differences:
--
-- * It cannot perform conditional decision making.
-- * It always returns the result that consumes the most input characters, adhering to the
--   "longest match" rule commonly used in lexical analysis.
newtype Lexer a = Lexer { runLexer :: String -> Either LexingError (a, String) }

-- | Instance of `Functor` for `Lexer`. Allows applying functions to the result of a lexer
-- without affecting the lexing process itself. The function is applied to the lexed value (`a`)
-- within the `Right` constructor of the `Either` value returned by the `Lexer`.
instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (first f))

-- | Instance of `Applicative` for `Lexer`. Allows combining lexers sequentially.
-- The first lexer (`lF`) consumes some input, and the second lexer (`lA`) consumes the
-- remaining input. The final result is a pair of the value obtained by applying `f` to the
-- result of the first lexer and the result of the second lexer.
instance Applicative Lexer where
  pure a = Lexer (\input -> Right (a, input))
  Lexer lF <*> Lexer lA =
    Lexer <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

-- | Instance of `Alternative` for `Lexer`. Allows choosing between lexers that successfully
-- matched some input. The chosen lexer will be the one that consumed the most characters
-- (longest match rule). The `empty` value represents a failed lexing attempt.
instance Alternative Lexer where
  empty = Lexer (Left <<< unexpected)
  Lexer lA <|> Lexer lB =
    Lexer <| \input -> case (lA input, lB input)  of -- Try both lexers on the input
      (res, Left _) -> res  -- If only one succeeds, use its result
      (Left _, res) -> res
      -- Implement the longest match rule here
      (a@(Right (_, restA)), b@(Right (_, restB))) ->  -- Both succeeded, choose the longest
        if length restA <= length restB then a else b


-- | Lexical matcher that succeeds if the character satisfies a predicate.
--
-- The `satisfies` function takes a predicate function `(Char -> Bool)` as input. This
-- predicate determines whether a character matches the criteria. The function returns a
-- `Lexer Char` monadic computation.
--
-- The monadic computation (`\case ... -> ...`) attempts to match a single character from
-- the input stream:
--
-- * `c : cs | p c -> Right (c, cs)`: If the first character (`c`) in the input stream
--   matches the predicate (`p c`) defined by the argument, the character is returned as a
--   `Right (c, cs)`. The remaining input (`cs`) is also returned, allowing for further
--   lexing. This connects to the concept of matching single characters in an EBNF grammar
--   rule.
-- * `rest -> Left (unexpected rest)`: If the first character doesn't match the predicate,
--   the function fails by returning `Left (unexpected rest)`. This indicates a lexing error
--   because an unexpected character was encountered.
satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer <| \case
    c : cs | p c -> Right (c, cs)
    rest -> Left (unexpected rest)

-- | Lexical matcher that succeeds if the character matches a specific value.
--
-- The `char` function takes a single `Char` value as input, representing the character to
-- match. It returns a `Lexer Char` monadic computation. This is essentially a shortcut for
-- using `satisfies` with a predicate that checks for character equality (`==`).
--
-- The monadic computation (`\case ... -> ...`) behaves similarly to `satisfies`:
--
-- * Success (`Right`): If the first character in the input stream matches the target character,
--   it's returned along with the remaining input.
-- * Failure (`Left`): If the characters don't match, a lexing error is returned.
--
-- This directly connects to matching specific characters (literals) in an EBNF grammar rule.
--
char :: Char -> Lexer Char
char target = satisfies (== target)

-- | Lexical matcher that succeeds if the entire input string matches a given string.
--
-- The `string` function takes a `String` value as input, representing the string to match.
-- It returns a `Lexer String` monadic computation. This function  uses `traverse`
-- internally to apply the `char` lexer to each character in the input string.
--
-- The monadic computation ( implemented using `traverse`) succeeds (`Right`) only if
-- all characters in the input string match the characters in the provided string, in the
-- same order. Otherwise, it fails (`Left`) with a lexing error.
--
-- This directly connects to matching keywords or identifiers (sequences of letters, digits,
-- and underscores) in an EBNF grammar rule.
string :: String -> Lexer String
string = traverse char

-- | Creates an alternation (OR) between a list of lexers.
--
-- The `anyOf` function takes a list of `f a` lexers, where `f` is a monad transformer type
-- supporting the `Alternative` type class constraint. This constraint ensures the existence
-- of an `(<|>)` operator for combining alternative lexers. The function returns an `f a`
-- monadic computation representing the alternation.
--
-- The function uses `foldl1'` (left fold with strict evaluation) to combine the lexers in the
-- list using the `(<|>)` operator. The `(<|>)` operator implements the alternation logic.
anyOf :: Alternative f => [f a] -> f a
anyOf = foldl1' (<|>)


-- | Data type representing a tinyHaskell token.
--
-- The `Token` data type defines the various kinds of tokens that the lexer can recognize
-- in tinyHaskell source code. Each variant of the data type corresponds to a specific lexical
-- element.
--
-- * Keywords: These are reserved words in tinyHaskell that have special meaning within the
--   language. The data type includes variants for `Let`, `Where`, `In`, `Data`, `Type`,
--   `If`, `Then`, `Else`, `Case`, and `Of`.
-- * Symbols: These are special characters used for various syntactic purposes. The data type
--   includes variants for common symbols like `OpenParens` (`(`), `CloseParens` (`)`),
--   `OpenBrace` (`{`), `CloseBrace` (`}`), `Semicolon` (`;`), `DoubleColon` (`::`), `ThinArrow`
--   (`->`), `VBar` (`|`), `BSlash` (`\`), `FSlash` (`/`), operators like `Plus` (`+`),
--   `Dash` (`-`), `Asterisk` (`*`), and comparison operators.
-- * Literals: These represent constant values directly embedded in the code. The data type
--   includes variants for `IntLit` (integer literals), `StringLit` (string literals), and
--   `BoolLit` (boolean literals).
-- * Type names: These are predefined types in tinyHaskell. The data type includes variants for
--   `IntTypeName`, `StringTypeName`, and `BoolTypeName`.
-- * Identifiers: These are user-defined names for variables, functions, or types. The data
--   type includes variants for `UpperName` (names starting with an uppercase character) and
--   `LowerName` (names starting with a lowercase character).
--
-- All variants of the `Token` data type derive `Eq` and `Show` typeclasses, allowing for
-- equality comparisons and easy string representation of tokens.
--
-- **Connection to EBNF Grammar:** This data type directly reflects the different categories
-- of lexical elements (keywords, symbols, literals, identifiers) that can appear in a tinyHaskell
-- EBNF grammar.
data Token
  = Let -- `let`
  | Where -- `where`
  | In -- `in`
  | Data -- `data`
  | Type -- `type`
  | If -- `if`
  | Then -- `then`
  | Else -- `else`
  | Case -- `case`
  | Of -- `of`
  | Underscore -- `_`
  | OpenParens -- `(`
  | CloseParens -- `)`
  | OpenBrace -- `{`
  | CloseBrace -- `}`
  | Semicolon -- `;`
  | DoubleColon -- `::`
  | ThinArrow -- `->`
  | VBar -- `|`
  | BSlash -- `\`
  | FSlash -- `/`
  | Plus -- `+`
  | PlusPlus -- `++`
  | Dash -- `-`
  | Asterisk -- `*`
  | Equal -- `=`
  | Dollar -- `$`
  | LeftAngle -- `<`
  | Dot -- `.`
  | LeftAngleEqual -- `<=`
  | RightAngle -- `>`
  | RightAngleEqual -- `>=`
  | EqualEqual -- `==`
  | FSlashEqual -- `/=`
  | VBarVBar -- `||`
  | AmpersandAmpersand -- `&&`
  | IntLit Int -- An Int literal
  | StringLit String -- A String literal
  | BoolLit Bool -- A Bool literal
  | IntTypeName -- The typename `Int`
  | StringTypeName -- The typename `String`
  | BoolTypeName -- The typename `Bool`
  | UpperName String -- A reference to a name beginning with an upper case
  | LowerName String -- A reference to a name beginning with a lower case
  deriving (Show, Eq)


-- | Lexes a single token from the input stream.
--
-- The `token` function is a parser combinator that attempts to lex a single token from the
-- input stream. It uses the `(<|>)` operator (orElse in some libraries) to combine several
-- parser combinator expressions in an alternation (OR) fashion. This means it will try
-- the first parser (`keyword`), and if that fails, it will try the second (`operator`), and
-- so on, until a successful parse occurs. The function returns a `Lexer (Token, String)`
-- monadic computation.
--
-- The success (`Right`) value of the monadic computation is a tuple containing:
--
-- * `Token`: The lexed token itself (e.g., `Let`, `IntLit 42`).
-- * `String`: The remaining input string after lexing the token. This allows for
--   chaining further lexing steps.
--
-- The function uses a helper function `with` to combine a fixed value (`Token`) with the
-- result of another parser (`String`). This is used to construct the final token tuple
-- after lexing a keyword string.
--
-- **Where clauses:** The `where` clauses define helper parser combinators:
--
-- * `with`: This function takes a fixed value (`b`) and a parser (`f a`) as arguments. It
--   applies the parser `f` and then uses `fmap` to combine the parsed value (`a`) with the
--   fixed value (`b`) into a tuple `(b, a)`. This is useful for attaching additional
--   information (like the token type) to the parsing result.
-- * `keyword`: This parser combinator attempts to lex a keyword from the input stream.
--   It uses `anyOf` to create an alternation of several parsers, each matching a specific
--   keyword and its corresponding string representation using `with`. This directly connects
--   to matching keywords in an EBNF grammar rule.
token :: Lexer (Token, String)
token = keyword <|> operator <|> literal <|> name
  where
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (b,)

    keyword :: Lexer (Token, String)
    keyword =
      anyOf
        [ Let `with` string "let",
          Where `with` string "where",
          In `with` string "in",
          Data `with` string "data",
          Type `with` string "type",
          If `with` string "if",
          Then `with` string "then",
          Else `with` string "else",
          Case `with` string "case",
          Of `with` string "of",
          Underscore `with` string "_"
        ]

    -- | Lexes a single operator from the input stream.
    --
    -- The `operator` function is a parser combinator that attempts to lex a single operator
    -- from the input stream. It uses `anyOf` to create an alternation of several parsers,
    -- each matching a specific operator symbol and its corresponding string representation
    -- using `with`. This function returns a `Lexer (Token, String)` monadic computation.
    --
    -- The success (`Right`) value of the monadic computation is a tuple containing:
    --
    -- * `Token`: The lexed operator token (e.g., `OpenParens`, `Plus`).
    -- * `String`: The remaining input string after lexing the operator.
    --
    -- This function directly connects to matching operator symbols in an EBNF grammar rule
    -- for expressions, statements, and other syntactic constructs.
    operator :: Lexer (Token, String)
    operator =
      anyOf
        [ OpenParens `with` string "(",
        CloseParens `with` string ")",
        OpenBrace `with` string "{",
        CloseBrace `with` string "}",
        Semicolon `with` string ";",
        DoubleColon `with` string "::",
        ThinArrow `with` string "->",
        VBar `with` string "|",
        BSlash `with` string "\\",
        FSlash `with` string "/",
        Plus `with` string "+",
        PlusPlus `with` string "++",
        Dash `with` string "-",
        Asterisk `with` string "*",
        Equal `with` string "=",
        Dot `with` string ".",
        Dollar `with` string "$",
        LeftAngle `with` string "<",
        LeftAngleEqual `with` string "<=",
        RightAngle `with` string ">",
        RightAngleEqual `with` string ">=",
        FSlashEqual `with` string "/=",
        EqualEqual `with` string "==",
        VBarVBar `with` string "||",
        AmpersandAmpersand `with` string "&&"
        ]
    
    -- | Lexes a single literal from the input stream.
    --
    -- The `literal` function is a parser combinator that attempts to lex a single literal
    -- from the input stream. It uses `(<|>)` (orElse in some libraries) for alternation to
    -- combine three parsers: `integerLiteral` for integer literals, `stringLiteral` for string literals,
    -- and `boolLiteral` for boolean literals. The function returns a `Lexer (Token, String)`
    -- monadic computation.
    --
    -- The success (`Right`) value of the monadic computation is a tuple containing:
    --
    -- * `Token`: The lexed literal token (e.g., `IntLit 42`, `StringLit "hello"`).
    -- * `String`: The remaining input string after lexing the literal.
    --
    -- This function directly connects to matching literals in an EBNF grammar rule. Literals
    -- can appear in various contexts like expressions, function arguments, and pattern
    -- matching.
    --
    -- **Where clauses:** The `where` clauses define helper parser combinators for specific
    -- literal types:
    --
    -- * `integerLiteral`: This parser lex an integer literal. It uses `some` to match one or more
    --   digits using the `isDigit` predicate. The result is then transformed using `fmap` to
    --   convert the string of digits into an `IntLit` token and return the original string
    --   for further processing. This directly connects to the concept of integer literals
    --   in an EBNF grammar rule.
    -- * `stringLiteral`: This parser lex a string literal. It starts with `char '"'` to match
    --   the opening double quote. Then, it uses `many` to match zero or more characters that
    --   are not double quotes (`/= '"'`). Finally, it matches the closing double quote with
    --   `char '"'`. The result is transformed using `fmap` to create a `StringLit` token and
    --   return the original string content. This directly connects to the concept of string
    --   literals enclosed in double quotes in an EBNF grammar rule.
    -- * `boolLiteral`: This parser lex a boolean literal. It uses alternation (`(<|>)`) to
    --   distinguish between the keywords "True" and "False". It combines the specific
    --   `BoolLit` token value with the string representation using `with`. This directly
    --   connects to matching the keywords "True" and "False" in an EBNF grammar rule for
    --   boolean literals.
    literal :: Lexer (Token, String)
    literal = integerLiteral <|> stringLiteral <|> boolLiteral
      where
        integerLiteral :: Lexer (Token, String)
        integerLiteral = some (satisfies isDigit) |> fmap (\x -> (IntLit (read x), x))

        stringLiteral :: Lexer (Token, String)
        stringLiteral = char '"' *> (many (satisfies (/= '"')) <* char '"') |> fmap (\x -> (StringLit x, x))

        boolLiteral :: Lexer (Token, String)
        boolLiteral = (BoolLit True `with` string "True") <|> (BoolLit False `with` string "False")

    -- | Lexes a single identifier or keyword starting with a special character from the input stream.
    --
    -- The `name` function is a parser combinator that attempts to lex a single identifier
    -- or keyword starting with a special character (like a single quote) from the input stream.
    -- It uses alternation (`(<|>)`) to combine three parsers: `primitiveTypeName` for predefined type
    -- keywords, `upperName` for identifiers starting with an uppercase letter, and
    -- `lowerName` for identifiers starting with a lowercase letter. The function returns a
    -- `Lexer (Token, String)` monadic computation.
    --
    -- The success (`Right`) value of the monadic computation is a tuple containing:
    --
    -- * `Token`: The lexed identifier token (e.g., `UpperName "myVar"`, `IntTypeName`).
    -- * `String`: The remaining input string after lexing the identifier or keyword.
    --
    -- This function directly connects to matching identifiers and special keywords that start
    -- with characters other than letters or underscores in an EBNC grammar rule. However, in
    -- tinyHaskell, most identifiers start with letters or underscores, which are handled elsewhere.
    --
    -- **Where clauses:** The `where` clauses define helper parser combinators for specific
    -- name-related lexing tasks:
    --
    -- * `continuesName`: This parser matches a character that can follow the first character
    --   in an identifier name. It allows alphanumeric characters and single quotes (`'`). This
    --   connects to the concept of identifier names potentially containing underscores and
    --   apostrophes.
    -- * `followedBy`: This parser combines two character parsers (`l1` and `l2`). It ensures
    --   that `l1` is matched first, followed by zero or more repetitions of `l2`. The results
    --   are combined into a string using `liftA2 (:)`. This is a general helper function
    --   useful for building parsers that match a prefix followed by a sequence.
    -- * `upperName`: This parser lex an identifier starting with an uppercase letter. It uses
    --   `satisfies isUpper` to match the first character and then uses `followedBy continuesName`
    --   to match zero or more characters that can follow the first uppercase letter. The result
    --   is transformed using `fmap` to create an `UpperName` token and return the original
    --   string content. This directly connects to matching identifiers starting with uppercase
    --   letters in an EBNF grammar rule.
    -- * `lowerName`: This parser lex an identifier starting with a lowercase letter. It's similar
    --   to `upperName` but uses `satisfies isLower` to match the first character. This directly
    --   connects to matching identifiers starting with lowercase letters in an EBNF grammar rule.
    -- * `primitiveTypeName`: This parser lex a predefined type keyword. It uses alternation (`(<|>)`)
    --   to distinguish between the keywords "Int", "String", and "Bool". It combines the
    --   specific `Token` variant with the string representation using `with`. This directly
    --   connects to matching the keywords for predefined types in an EBNF grammar rule.
    name :: Lexer (Token, String)
    name = primitiveTypeName <|> upperName <|> lowerName
      where
        continuesName :: Lexer Char
        continuesName = satisfies isAlphaNum <|> char '\''

        followedBy :: Lexer Char -> Lexer Char -> Lexer String
        followedBy l1 l2 = liftA2 (:) l1 (many l2)

        upperName :: Lexer (Token, String)
        upperName = (satisfies isUpper `followedBy` continuesName) |> fmap (\x -> (UpperName x, x))

        lowerName :: Lexer (Token, String)
        lowerName = (satisfies isLower `followedBy` continuesName) |> fmap (\x -> (LowerName x, x))

        primitiveTypeName :: Lexer (Token, String)
        primitiveTypeName =
            (IntTypeName `with` string "Int")
            <|> (StringTypeName `with` string "String")
            <|> (BoolTypeName `with` string "Bool")

-- | Represents a raw token encountered during the lexing process.
--
-- The `RawToken` data type categorizes the different types of elements that can be
-- identified during the initial lexing phase. It serves as a temporary representation
-- before filtering out irrelevant elements (like whitespace) and converting meaningful
-- elements into proper `Token` types.
--
-- * `Blankspace`: This variant represents a sequence of whitespace characters (excluding
--   newlines). The constructor takes a `String` argument containing the actual whitespace
--   characters encountered.
-- * `Comment`: This variant represents a line comment starting with "--". The constructor
--   takes a `String` argument containing the comment text (excluding the "--" prefix).
-- * `Newline`: This variant simply represents a newline character (`\n`).
-- * `TokenWithPosition`: This variant represents a "real" token that carries semantic meaning
--   in the language. It combines two values: a `Token` from the `Token` data type (which
--   represents the actual token type like `Let`, `IntLit`, etc.) and a `String` containing
--   the original string representation of the token in the source code. This allows for
--   further processing or error handling based on the original string if needed.
--
data RawToken
  = Blankspace String
  | Comment String
  | Newline
  | TokenWithPosition Token String

-- | Lexes the input stream into a list of `RawToken`s.
--
-- The `rawLexer` function is a parser combinator that lexes the entire input stream
-- and produces a list of `RawToken`s. It uses the `some` parser combinator to ensure that
-- at least one element is matched. The function applies an alternation (`(<|>)`) to handle
-- different categories of elements:
--
-- * `whitespace`: This parser combines `blankspace` and `newline` parsers using
--   alternation.
-- * `comment`: This parser matches line comments starting with "--" and captures the
--   comment text.
-- * `token`: This parser lex actual tokens using the previously defined `token`
--   function (assumed to exist elsewhere). The result is wrapped in `fmap (uncurry TokenWithPosition)`
--   to convert the parser output (a tuple of `Token` and `String`) into a `TokenWithPosition`.
--
-- This initial lexing phase separates whitespace and comments from actual tokens for
-- further processing. The specific EBNF grammar rules for whitespace and comments may vary
-- depending on the tinyHaskell dialect, but this implementation handles common cases.
--
-- **Where clauses:** The `where` clauses define helper parser combinators for specific
-- lexing tasks during the raw tokenization phase:
--
-- * `whitespace`: This parser combines `blankspace` and `newline` parsers using
--   alternation.
-- * `blankspace`: This parser matches a sequence of whitespace characters (excluding
--   newlines). It uses `some` to ensure at least one whitespace character is present.
-- * `comment`: This parser matches line comments starting with "--". It uses `string "--"`
--   to match the prefix and then uses `many` to capture zero or more characters until
--   the end of the line (represented by `/= '\n'`).
-- * `newline`: This parser simply matches a newline character (`\n`).
rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> fmap (uncurry TokenWithPosition) token)
  where
    whitespace = blankspace <|> newline
    blankspace = Blankspace <$> some (satisfies (\x -> isSpace x && x /= '\n'))
    comment = Comment <$> (string "--" *> many (satisfies (/= '\n')))
    newline = Newline <$ char '\n'

-- | Represents the position of a token within a line of source code.
--
-- The `LinePosition` data type defines two possible positions for a token within a line:
--
-- * `Start`: This variant indicates that the token is located at the beginning of the line.
-- * `Middle`: This variant indicates that the token appears somewhere in the middle of the
--   line, not necessarily at the very beginning.
--
-- This information can be useful for error reporting or code generation tasks that might
-- be sensitive to token placement within a line.
data LinePosition = Start | Middle
  deriving (Show, Eq)

-- | Represents a value annotated with its position within the source code.
--
-- The `Positioned` data type combines a value of some arbitrary type `a` with its position
-- information. This position information includes:
--
-- * `LinePosition`: The position of the value within the line (either `Start` or `Middle`).
-- * `Int`: The column number within the line where the value starts.
--
-- This is a generic type that can be used to store positioned tokens, comments, or any
-- other elements in the source code that require positional information.
data Positioned a = Positioned a LinePosition Int
  deriving (Show)

-- | Type alias for the internal state used during token positioning.
--
-- The `LexerState` type alias defines a tuple that represents the internal state used
-- during the `position` function. This state consists of two elements:
--
-- * `LinePosition`: The current line position (either `Start` or `Middle`).
-- * `Int`: The current column number within the line.
--
-- This state is not directly exposed to the user and is used internally by the
-- `position` function to track its progress while processing raw tokens.
type LexerState = (LinePosition, Int)

-- | Processes a list of raw tokens and removes whitespace, returning positioned tokens.
--
-- The `position` function takes a list of `RawToken`s (containing tokens, whitespace,
-- and comments) and processes them to produce a list of `Positioned Token`s. It uses
-- `foldl'` to accumulate the results while processing the list of `RawToken`s. It
-- applies the helper function `go` to each element and accumulates the positioned tokens
-- in a list. Finally, it reverses the accumulated list (since processing starts from
-- the beginning) and extracts only the positioned tokens using `snd` and `maybeToList`.
--
-- This function effectively filters out whitespace and comments while assigning line position
-- and column information to actual tokens. The specific EBNF grammar rules for whitespace
-- and comments may vary depending on the tinyHaskell dialect, but this implementation handles
-- common cases.
--
-- **Where clauses:** The `where` clauses define helper functions used during the token
-- positioning process:
--
-- * `eat`: This function takes the current state (`LexerState`) and a `RawToken` and
--   processes it. It handles different cases of `RawToken`:
--     * `Newline` and `Comment`: These tokens are discarded, and the position is reset
--       to `(Start, 0)`.
--     * `Blankspace`: The column position is incremented by the length of the whitespace
--       string, but no positioned token is produced.
--     * `TokenWithPosition`: A `Positioned Token` is created with the token itself, the current
--       line position, and the current column position. The column position is then
--       updated based on the length of the token string.
-- * `go`: This function is the fold accumulator. It takes the current state (`LexerState`)
--   and a `RawToken`, applies the `eat` function to process the token, and updates the
--   state and accumulated list of positioned tokens.
position :: [RawToken] -> [Positioned Token]
position = foldl' go ((Start, 0), []) >>> snd >>> reverse
  where
    eat :: LexerState -> RawToken -> (LexerState, Maybe (Positioned Token))
    eat (pos, col) = \case
      Newline -> ((Start, 0), Nothing)
      Comment _ -> ((Start, 0), Nothing)
      Blankspace s -> ((pos, col + length s), Nothing)
      TokenWithPosition t s -> ((Middle, col + length s), Just (Positioned t pos col))
    go :: (LexerState, [Positioned Token]) -> RawToken -> (LexerState, [Positioned Token])
    go (p, acc) raw =
      let (p', produced) = eat p raw
       in (p', maybeToList produced <> acc)


-- | Represents the layout context for a token.
--
-- The `Layout` data type defines two possible layout contexts for a token:
--
-- * `Explicit`: This variant indicates that the layout for this token was explicitly
--   declared by the user (potentially using indentation or layout directives).
-- * `Implicit Int`: This variant indicates that the layout for this token is implicit
--   and is determined by its column position within the line (represented by the `Int`
--   value). This is often used for indentation-based layout styles.
--
-- Layout can be a crucial aspect of code readability and maintainability. This information
-- can be used during the code generation phase to format the generated C code according
-- to the desired layout style.
data Layout = Explicit | Implicit Int

-- | Represents the state during token layout processing.
--
-- The `LayoutState` data type encapsulates the state information required for processing
-- token layout. It includes:
--
-- * `layouts`: A stack of currently active layout contexts. This allows for nested
--   layout declarations, where inner declarations can override outer ones.
-- * `tokens`: The remaining stream of tokens to be processed for layout.
-- * `expectingLayout`: A flag indicating whether the next token is expected to be the
--   start of a new layout declaration. This helps guide the layout processing logic.
--
-- This state information is used by the layout processing functions to determine the
-- appropriate layout context for each token during the code generation process.
data LayoutState = LayoutState
  { layouts :: [Layout],
    tokens :: [Token],
    expectingLayout :: Bool
  }

-- | Monad transformer for layout processing.
--
-- The `LayoutM` type alias defines a monad transformer for layout processing. It
-- combines two monad transformers:
--
-- * `ExceptT LexingError`: This handles potential errors that might occur during the
--   layout processing, such as invalid layout directives or unexpected token sequences.
-- * `State LayoutState`: This allows for maintaining and modifying the layout state
--   during the processing of tokens.
--
-- This monad transformer provides a convenient way to handle errors and state management
-- during the layout processing phase.
type LayoutM a = ExceptT LexingError (State LayoutState) a


-- | Signals the production of a token during layout processing.
--
-- The `yieldToken` function takes a `Token` as input and modifies the internal layout
-- state using the `modify'` function from the `State` monad transformer. It appends the
-- provided token to the front (`(:)`) of the `tokens` list within the `LayoutState`.
-- This effectively adds the token to the stream of tokens being processed for layout.
yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

-- | Pushes a new layout context onto the layout stack.
--
-- The `pushLayout` function takes a `Layout` value representing the new layout context
-- and modifies the internal layout state using the `modify'` function from the `State`
-- monad transformer. It prepends (`(:)`) the new layout to the `layouts` list within the
-- `LayoutState`. This effectively makes the new layout the active layout context for
-- subsequent tokens.
pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

-- | Removes (pops) the top layout context from the layout stack.
--
-- The `popLayout` function modifies the internal layout state using the `modify'` function
-- from the `State` monad transformer. It applies `drop 1` to the `layouts` list within the
-- `LayoutState`, effectively removing the top element (most recent layout). This function
-- has no effect if the layout stack is empty (`layouts s` is empty).
popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = drop 1 (layouts s)})

-- | Retrieves the currently active layout context, if any.
--
-- The `currentLayout` function uses the `gets` function from the `State` monad transformer
-- to access the `layouts` list within the `LayoutState`. It then applies `fmap listToMaybe`
-- to transform the potentially empty list into a `Maybe Layout` value. This allows for
-- handling the case where the layout stack is empty.
currentLayout :: LayoutM (Maybe Layout)
currentLayout = gets layouts |> fmap listToMaybe


-- | Compares a given indentation level with the currently active layout context.
--
-- The `compareIndentation` function takes an `Int` representing the indentation column
-- and compares it with the current layout context retrieved using `currentLayout`. It uses a
-- local helper function to handle different cases of the current layout:
--
-- * `Nothing`: If there is no active layout context (empty layout stack), the indentation
--   is considered greater (`GT`) using the `compare` function.
-- * `Just Explicit`: If the current layout is explicit (declared layout), the indentation
--   is considered greater (`GT`) regardless of the column value. This is because explicit
--   layouts override indentation-based layouts.
-- * `Just (Implicit n)`: If the current layout is implicit (based on indentation), the
--   indentation column (`col`) is compared with the stored column value (`n`) in the
--   `Implicit` constructor using the `compare` function.
--
-- The result of the comparison (`Ordering`) is then wrapped in a `Maybe` using `fmap cmp`
-- to handle the case where there is no current layout.
compareIndentation :: Int -> LayoutM Ordering
compareIndentation col =
  let cmp Nothing = GT
      cmp (Just Explicit) = GT
      cmp (Just (Implicit n)) = compare col n
   in fmap cmp currentLayout

-- | Executes the layout processing monad computation and returns the final tokens.
--
-- The `runLayoutM` function takes a `LayoutM a` computation (potentially involving layout
-- manipulation and token processing) and runs it within the `ExceptT` and `State` monad
-- transformers. It uses pattern matching on the final result:
--
-- * `Left e`: If an error (`LexingError`) occurred during layout processing, it is wrapped
--   in a `Left` constructor and returned as the final result.
-- * `Right _`: If the computation was successful, the right value is further destructured
--   using pattern matching on the internal state (`LayoutState`). It discards the layout
--   information and extracts the final list of tokens (`ts`) in reverse order (since tokens
--   were processed in a stack-like manner) using `reverse`. The reversed list of tokens is
--   then wrapped in a `Right` constructor and returned as the final result.
runLayoutM :: LayoutM a -> Either LexingError [Token]
runLayoutM =
  runExceptT >>> (`runState` LayoutState [] [] True) >>> \case
    (Left e, _) -> Left e
    (Right _, LayoutState _ ts _) -> Right (reverse ts)

-- | Inserts semicolons and braces judiciously into a stream of positioned tokens.
--
-- The `layout` function takes a list of `Positioned Token`s representing the raw token stream
-- with line position information and processes it to potentially insert semicolons and braces.
-- It uses the `runLayoutM` function to execute the layout processing logic within the
-- `LayoutM` monad transformer. The monadic computation performs the following steps:
--
-- 1. `mapM_ step inputs`: This iterates over each `Positioned Token` in the `inputs` list
--   and applies the `step` function to process each token.
-- 2. `closeImplicitLayouts`: This function (assumed to exist elsewhere) handles closing
--   any implicitly opened layout contexts at the end of the token stream.
--
-- The `where` clause defines helper functions used during the layout processing:
--
-- * `isLayoutStart`: This function takes a `Token` and checks if it belongs to a set of tokens
--   (like `Let`, `Where`, or `Of`) that typically start a layout block in tinyHaskell. It uses
--   `elem` to check for membership in a list. This function helps identify potential starting
--   points for layout contexts.
-- * `step`: This function takes a `Positioned Token` and performs the main logic for
--   inserting semicolons and braces based on the token type, line position, and current
--   layout state:
--     * `CloseBrace`: If the token is a `CloseBrace`, it triggers the `closeExplicitLayout`
--       function (assumed to exist elsewhere) to handle closing an explicitly declared layout.
--     * `OpenBrace`: If the token is an `OpenBrace` and the `expectingLayout` flag is set
--       (indicating an expected layout start), it triggers the `startExplicitLayout` function
--       (assumed to exist elsewhere) to handle creating an explicit layout context.
--     * Other tokens:
--       * If the token belongs to the set identified by `isLayoutStart`, it sets the
--         `expectingLayout` flag to `True` using `modify'` to indicate a potential layout
--         start for the next token.
--       * If the `expectingLayout` flag is set:
--         * It calls `startImplicitLayout` (assumed to exist elsewhere) to potentially
--           start an implicit layout context based on the current column (`col`).
--       * If the line position is `Start` (beginning of a line), it calls
--         `continueImplicitLayout` (assumed to exist elsewhere) to potentially continue
--         an existing implicit layout context based on the current column (`col`).
--     In all other cases, the function simply returns using `return ()`.
-- * `yieldToken`: This function (defined previously) is used to emit the processed token
--   after any layout handling has been performed.
--
-- This function aims to add necessary semicolons and braces to improve the structure and
-- readability of the generated C code, mimicking the block structure of tinyHaskell code. The
-- specific rules for semicolon and brace insertion might vary depending on the chosen tinyHaskell
-- dialect or coding conventions.
layout :: [Positioned Token] -> Either LexingError [Token]
layout inputs =
  runLayoutM <| do
    mapM_ step inputs
    closeImplicitLayouts
    where
        isLayoutStart :: Token -> Bool
        isLayoutStart = (`elem` [Let, Where, Of])

        step :: Positioned Token -> LayoutM ()
        step (Positioned t linePos col) = do
          expectingLayout' <- gets expectingLayout
          case t of
            CloseBrace -> closeExplicitLayout
            OpenBrace | expectingLayout' -> startExplicitLayout
            _
              | isLayoutStart t -> modify' (\s -> s {expectingLayout = True})
              | expectingLayout' -> startImplicitLayout col
              | linePos == Start -> continueImplicitLayout col
              | otherwise -> return ()
          yieldToken t

        -- | Closes the topmost explicitly declared layout context.
        --
        -- The `closeExplicitLayout` function operates within the `LayoutM` monad transformer. It
        -- first retrieves the current layout context using `currentLayout`. It then uses pattern
        -- matching to handle different cases:
        --
        -- * `Just Explicit`: If the current layout is `Explicit` (declared layout), it calls
        --   `popLayout` to remove this layout context from the layout stack. This effectively
        --   closes the explicitly declared layout block.
        -- * `_`: In any other case (including `Nothing` or `Just (Implicit n)`), the function
        --   throws an error using `throwError` to indicate an unexpected closing brace (`'}`)
        --   encountered where an explicit layout was not open. This helps enforce balanced and
        --   well-formed layout structures.
        closeExplicitLayout :: LayoutM ()
        closeExplicitLayout =
          currentLayout >>= \case
              Just Explicit -> popLayout
              _ -> throwError (UnexpectedChar '}')

        -- | Starts a new explicitly declared layout context.
        --
        -- The `startExplicitLayout` function operates within the `LayoutM` monad transformer. It
        -- performs the following actions:
        --
        -- 1. `modify' (\s -> s {expectingLayout = False})`: This resets the `expectingLayout` flag
        --   to `False` using `modify'`. This indicates that an explicit layout has now been declared,
        --   and we no longer expect another layout start at the next token.
        -- 2. `pushLayout Explicit`: This pushes a new `Explicit` layout context onto the layout stack
        --   using `pushLayout`. This marks the beginning of a new explicitly declared layout block.
        startExplicitLayout :: LayoutM ()
        startExplicitLayout = do
          modify' (\s -> s {expectingLayout = False})
          pushLayout Explicit

        -- | Starts a new implicitly declared layout context based on indentation.
        --
        -- The `startImplicitLayout` function operates within the `LayoutM` monad transformer. It takes
        -- an `Int` representing the current column position (`col`) and performs the following
        -- actions:
        --
        -- 1. `modify' (\s -> s {expectingLayout = False})`: This resets the `expectingLayout` flag
        --   to `False` using `modify'`. This indicates that an implicit layout has now been started
        --   based on indentation, and we no longer expect another layout start at the next token.
        -- 2. `compareIndentation col >>= \case GT -> ...`: It calls `compareIndentation` with the
        --   current column (`col`) to compare it with the active layout. It uses pattern matching
        --   on the comparison result:
        --     * `GT`: This indicates that the current indentation level is greater than any active
        --       layout. This is considered the start of a new block. The function then:
        --         * `yieldToken OpenBrace`: It emits an `OpenBrace` token to mark the beginning
        --           of the new implicit layout block.
        --         * `pushLayout (Implicit col)`: It pushes a new `Implicit col` layout context onto
        --           the layout stack using `pushLayout`. This stores the current column as the
        --           reference for the implicit layout indentation level.
        --
        startImplicitLayout :: Int -> LayoutM ()
        startImplicitLayout col = do
          modify' (\s -> s {expectingLayout = False})
          -- Regardless of what happens, we're starting a layout...
          compareIndentation col >>= \case
            GT -> do
              yieldToken OpenBrace
              pushLayout (Implicit col)
            -- But if we're not indented further, we're immediately ending that layout.
            -- Furthermore, we might be continuing an implicit layout.
            _ -> do
              yieldToken OpenBrace
              yieldToken CloseBrace
              continueImplicitLayout col

        -- | Continues an existing implicitly declared layout context based on indentation.
        --
        -- The `continueImplicitLayout` function operates within the `LayoutM` monad transformer. It
        -- takes an `Int` representing the current column position (`col`) and assumes an implicit
        -- layout context is already active. It performs the following actions:
        --
        -- 1. `closeFurtherLayouts`: This calls a helper function (assumed to exist elsewhere)
        --   that handles closing any layout contexts that are indented further than the current
        --   column (`col`). This ensures proper nesting of layout blocks based on indentation.
        -- 2. `compareIndentation col >>= \case`: It calls `compareIndentation` with the current
        --   column (`col`) to compare it with the active layout. It uses pattern matching on the
        --   comparison result:
        --     * `EQ`: This indicates that the current indentation level is equal to the active
        --       implicit layout. The function then:
        --         * `yieldToken Semicolon`: It emits a `Semicolon` token to potentially separate
        --           statements within the same indentation level.
        --     * `_`: In any other case (including `GT` or `LT`), the function simply returns using
        --       `return ()`. This might be because the indentation has changed, indicating a
        --       different layout context, or because a semicolon is not necessary at this point.
        continueImplicitLayout :: Int -> LayoutM ()
        continueImplicitLayout col = do
          closeFurtherLayouts
          compareIndentation col >>= \case
              EQ -> yieldToken Semicolon
              _ -> return ()
          where
              closeFurtherLayouts =
                compareIndentation col >>= \case
                    LT -> do
                      yieldToken CloseBrace
                      popLayout
                      closeFurtherLayouts
                    _ -> return ()
        
        -- | Closes any remaining implicitly declared layout contexts at the end of the token stream.
        --
        -- The `closeImplicitLayouts` function operates within the `LayoutM` monad transformer. It
        -- ensures that all implicitly declared layout contexts (`Implicit`) are properly closed by
        -- the end of the token stream. It performs the following actions:
        --
        -- 1. `currentLayout >>= \case`: It retrieves the current layout context using
        --   `currentLayout`. It then uses pattern matching to handle different cases:
        --     * `Nothing`: If there is no active layout context (empty layout stack), the function
        --       simply returns using `return ()` as there's nothing to close.
        --     * `Just Explicit`: If the current layout is `Explicit` (declared layout), it throws an
        --       error using `throwError` with the `UnmatchedLayout` constructor. This indicates that
        --       an explicitly declared layout block was opened but never closed, potentially leading to
        --       unbalanced code structure.
        --     * `Just (Implicit _)`: If the current layout is `Implicit`, it treats it as an
        --       unclosed implicit layout and closes it:
        --         * `yieldToken CloseBrace`: It emits a `CloseBrace` token to mark the end of the
        --           implicit layout block.
        --         * `popLayout`: It removes the `Implicit` layout context from the layout stack using
        --           `popLayout`.
        --         * `closeImplicitLayouts`: It recursively calls itself (`closeImplicitLayouts`) to
        --           check for any further implicitly declared layout contexts that might need closing.
        --           This ensures proper nesting of implicit layout blocks.
        --
        closeImplicitLayouts :: LayoutM ()
        closeImplicitLayouts =
          currentLayout >>= \case
              Nothing -> return ()
              Just Explicit -> throwError UnmatchedLayout
              Just (Implicit _) -> do
                yieldToken CloseBrace
                popLayout
                closeImplicitLayouts


-- | Lexes a given input string, performing tokenization, positioning, and layout processing.
--
-- The `lexer` function takes a `String` representing the tinyHaskell source code as input. It
-- returns an `Either LexingError [Token]` value. This means the function can either return:
--
-- * `Left LexingError`: An error value of type `LexingError` if an error occurs during
--   lexing (e.g., encountering an unexpected character or invalid token).
-- * `Right [Token]`: A list of `Token`s representing the lexed tokens if successful.
--
-- The function performs the following steps using monadic composition:
--
-- 1. `runLexer rawLexer input`: This calls the `runLexer` function (assumed to exist
--   elsewhere) with the `rawLexer` function (also assumed to exist elsewhere) and the input
--   string. The `rawLexer` function  performs the basic tokenization of the input
--   string, generating a list of unpositioned tokens.
-- 2. `fst`: This extracts the first element (`Left` or `Right`) from the result of
--   `runLexer`. This is because `runLexer`  returns a monadic computation that might
--   involve error handling. We only care about the final result (tokens or error).
-- 3. `position`: This applies the `position` function (assumed to exist elsewhere) to the
--   extracted value. The `position` function  takes a list of unpositioned tokens and
--   adds line and column position information to each token, creating a list of
--   `PositionedToken`s.
-- 4. `layout`: This applies the `layout` function (already documented previously) to the
--   list of `PositionedToken`s. The `layout` function inserts semicolons and braces
--   judiciously based on the token types, line positions, and layout context to improve the
--   structure and readability of the generated C code.
lexer :: String -> Either LexingError [Token]
lexer input =
  runLexer rawLexer input >>= (fst >>> position >>> layout)