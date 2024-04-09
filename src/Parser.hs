{-# LANGUAGE LambdaCase #-}

module Parser
  ( AST (..),
    Definition (..),
    DataVariant (..),
    ValueDefinition (..),
    Value (..),
    ValueIdentifier,
    ConstructorName,
    Identifier,
    BinaryOperator (..),
    Match (..),
    SimplifiedExpression (..),
    parser,
  )
where

import Control.Applicative (Alternative (..), liftA2)
import Lexer (Token (..))
import Ourlude
import Data.List (foldl')
import Types (Type (..), TypeName, TypeArgument)


type ValueIdentifier = String

type ConstructorName = String

type Identifier = String

newtype AST = AST [Definition] deriving (Show, Eq)

data Definition
  = ValueDefinition ValueDefinition
  | DataDefinition TypeName [TypeArgument] [DataVariant]
  | TypeSynonym TypeName Type
  deriving (Show, Eq)

data DataVariant = DataVariant ConstructorName [Type] deriving (Show, Eq)

data ValueDefinition
  = TypeAnnotation ValueIdentifier Type
  | NameDefinition ValueIdentifier [Match] SimplifiedExpression
  deriving (Show, Eq)

data SimplifiedExpression
  = LiteralExpression Value
  | IdentifierExpr Identifier
  | LambdaExpression [ValueIdentifier] SimplifiedExpression
  | CaseExpression SimplifiedExpression [(Match, SimplifiedExpression)]
  | WhereExpression SimplifiedExpression [ValueDefinition]
  | FunctionApplicationExpression SimplifiedExpression [SimplifiedExpression]
  | LetExpression [ValueDefinition] SimplifiedExpression
  | BinaryExpression BinaryOperator SimplifiedExpression SimplifiedExpression
  | IfThenElseExpression SimplifiedExpression SimplifiedExpression SimplifiedExpression
  | NegateExpression SimplifiedExpression
  deriving (Show, Eq)

data Value
  = StringLiteral String
  | IntegerLiteral Int
  | BooleanLiteral Bool
  deriving (Eq, Ord, Show)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Cash
  | Less
  | GreaterEqual
  | EqualTo
  | Div
  | Compose
  | NotEqualTo
  | And
  | Concat
  | LessEqual
  | Greater
  | Or
  deriving (Show, Eq)

data Match
  = WildcardPattern
  | ConstructorPattern ConstructorName [Match]
  | LiteralPattern Value
  | NamePattern ValueIdentifier
  deriving (Show, Eq)


data ParseError = ParserError | AmbiguousSyntax [(AST, [Token])] deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]}

instance Functor Parser where
  fmap f (Parser p) = Parser (p >>> fmap (first f))

instance Applicative Parser where
  pure a = Parser (\input -> [(a, input)])
  Parser lF <*> Parser lA =
    Parser <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser lA <|> Parser lB =
    Parser <| \input -> lA input ++ lB input

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser <| \case
    t : ts -> case f t of
      Just res -> [(res, ts)]
      _ -> []
    _ -> []

satisifies :: (Token -> Bool) -> Parser Token
satisifies p =
  Parser <| \case
    t : ts | p t -> [(t, ts)]
    _ -> []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

token :: Token -> Parser Token
token = (==) >>> satisifies

opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
opsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash start annotated =
      let (start', annotated') = foldl' shift (start, []) annotated
          shift (oldStart, stack) (combine, a) = (a, (combine, oldStart) : stack)
       in foldl' (\acc (combine, a) -> combine a acc) start' annotated'

betweenParentheses :: Parser a -> Parser a
betweenParentheses p = token OpenParens *> p <* token CloseParens

betweenBraces :: Parser a -> Parser [a]
betweenBraces p = token OpenBrace *> sepBy1 p (token Semicolon) <* token CloseBrace


abstractSyntaxTree :: Parser AST
abstractSyntaxTree = fmap AST (betweenBraces definition)

definition :: Parser Definition
definition = fmap ValueDefinition valueDefinition <|> dataDefinition <|> typeSynonym
  where
    dataDefinition =
      DataDefinition
        <$> (token Data *> typeName)
        <*> many typeVariable
        <*> (token Equal *> sepBy1 constructorDefinition (token VBar))
    typeSynonym = liftA2 TypeSynonym (token Type *> typeName) (token Equal *> typeExpression)

constructorDefinition :: Parser DataVariant
constructorDefinition = liftA2 DataVariant constructorName (many typeArgument)

valueDefinition :: Parser ValueDefinition
valueDefinition = nameDefinition <|> typeAnnotation
  where
    nameDefinition = NameDefinition <$> valueName <*> many unspacedPattern <*> (token Equal *> expr)
    typeAnnotation = liftA2 TypeAnnotation (valueName <* token DoubleColon) typeExpression

typeExpression :: Parser Type
typeExpression = opsR ((:->) <$ token ThinArrow) baseType
  where
    baseType = singleType <|> customType
    customType = liftA2 CustomType typeName (many typeArgument)

typeArgument :: Parser Type
typeArgument = namedType <|> singleType
  where
    namedType = fmap (`CustomType` []) typeName

singleType :: Parser Type
singleType = fmap TVar typeVariable <|> primType <|> betweenParentheses typeExpression
  where
    primType =
      (IntT <$ token IntTypeName)
        <|> (StringT <$ token StringTypeName)
        <|> (BoolT <$ token BoolTypeName)

expr :: Parser SimplifiedExpression
expr = notWhereExpression <|> whereExpression
  where
    lambdaExpression = token BSlash *> liftA2 LambdaExpression (some valueName) (token ThinArrow *> expr)
    letExpresssion = token Let *> liftA2 LetExpression (betweenBraces valueDefinition) (token In *> expr)
    whereExpression = liftA2 WhereExpression notWhereExpression (token Where *> betweenBraces valueDefinition)
    notWhereExpression = letExpresssion <|> ifThenElseExpression <|> lambdaExpression <|> binaryExpression <|> caseExpression    
    ifThenElseExpression = IfThenElseExpression <$> (token If *> expr) <*> (token Then *> expr) <*> (token Else *> expr)

literal :: Parser Value
literal = integerLiteral <|> stringLiteral <|> boolLiteral
  where
    boolLiteral =
      pluck <| \case
        BoolLit b -> Just (BooleanLiteral b)
        _ -> Nothing
    stringLiteral =
      pluck <| \case
        StringLit s -> Just (StringLiteral s)
        _ -> Nothing
    integerLiteral =
      pluck <| \case
        IntLit i -> Just (IntegerLiteral i)
        _ -> Nothing

name :: Parser Identifier
name = valueName <|> constructorName

typeName :: Parser TypeName
typeName = upperName

valueName :: Parser ValueIdentifier
valueName = lowerName

upperName :: Parser TypeName
upperName =
  pluck <| \case
    UpperName n -> Just n
    _ -> Nothing

lowerName :: Parser ValueIdentifier
lowerName =
  pluck <| \case
    LowerName n -> Just n
    _ -> Nothing

typeVariable :: Parser TypeArgument
typeVariable = lowerName

constructorName :: Parser ConstructorName
constructorName = upperName

unaryExpression :: Parser SimplifiedExpression
unaryExpression = negateExpr <|> functionApplicationExpression
  where
    negateExpr = (token Dash *> functionApplicationExpression) |> fmap NegateExpression

binaryExpression :: Parser SimplifiedExpression
binaryExpression = cashExpression
  where
    concatExpression = opsL (BinaryExpression Concat <$ token PlusPlus) addSubtractExpression
    functionCompositionExpression = opsR (BinaryExpression Compose <$ token Dot) unaryExpression
    orExpression = opsR (BinaryExpression Or <$ token VBarVBar) andExpression
    andExpression = opsR (BinaryExpression And <$ token AmpersandAmpersand) comparisonExpression
    cashExpression = opsR (BinaryExpression Cash <$ token Dollar) orExpression
    comparisonExpression = opsL comparisonOperator concatExpression
      where
        comparisonOperator =
          (BinaryExpression Less <$ token LeftAngle)
            <|> (BinaryExpression Greater <$ token RightAngle)
            <|> (BinaryExpression GreaterEqual <$ token RightAngleEqual)
            <|> (BinaryExpression EqualTo <$ token EqualEqual)
            <|> (BinaryExpression LessEqual <$ token LeftAngleEqual)
            <|> (BinaryExpression NotEqualTo <$ token FSlashEqual)
    addSubtractExpression = opsL addSubOperator multiplyDivideExpression
      where
        addSubOperator =
          (BinaryExpression Add <$ token Plus)
            <|> (BinaryExpression Sub <$ token Dash)
    multiplyDivideExpression = opsL muliplicationDivisionOperator functionCompositionExpression
      where
        muliplicationDivisionOperator =
          (BinaryExpression Mul <$ token Asterisk)
            <|> (BinaryExpression Div <$ token FSlash)

oneFunctionPattern :: Parser Match
oneFunctionPattern = unspacedPattern <|> argfulPattern
  where
    argfulPattern = liftA2 ConstructorPattern constructorName (some unspacedPattern)

caseExpression :: Parser SimplifiedExpression
caseExpression = liftA2 CaseExpression (token Case *> expr <* token Of) (betweenBraces patternDef)
  where
    patternDef = liftA2 (,) oneFunctionPattern (token ThinArrow *> expr)

unspacedPattern :: Parser Match
unspacedPattern = simplePattern <|> betweenParentheses oneFunctionPattern
  where
    literalPattern = fmap LiteralPattern literal
    variablePattern = fmap NamePattern valueName
    simplePattern = wildCardPattern <|> variablePattern <|> literalPattern <|> singleConstructor
    wildCardPattern = WildcardPattern <$ token Underscore
    singleConstructor = fmap (`ConstructorPattern` []) constructorName

functionApplicationExpression :: Parser SimplifiedExpression
functionApplicationExpression = some factor |> fmap extract
  where
    extract [] = error "functionApplicationExpression"
    extract [e] = e
    extract (e : es) = FunctionApplicationExpression e es

factor :: Parser SimplifiedExpression
factor = literalExpression <|> nameExpression <|> betweenParentheses expr
  where
    literalExpression = fmap LiteralExpression literal
    nameExpression = fmap IdentifierExpr name

parser :: [Token] -> Either ParseError AST
parser input = case runParser abstractSyntaxTree input of
  [] -> Left ParserError
  [(res, _)] -> Right res
  tooMany -> Left (AmbiguousSyntax tooMany)

