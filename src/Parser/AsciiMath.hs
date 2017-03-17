-- Parsing AsciiMath
--------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Parser.AsciiMath where

--------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number
import Value.Variable
import Value.Vector3D
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Radicalizable
import Constraint.VectorSpace
import Constraint.MeasureSpace
import Constraint.InnerProductSpace

import Data.Maybe
import Data.List

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

--------------------------------------------------------------------------------

-- BACKUS-NAUR FORM

-- c ::= [A-z] | numbers | greek letters | other constant symbols (see below)
-- u ::= 'sqrt' | 'text' | 'bb' |     other unary symbols for font commands
-- b ::= 'frac' | 'root' | 'stackrel' binary symbols
-- l ::= ( | [ | { | (: | {:          left brackets
-- r ::= ) | ] | } | :) | :}          right brackets
-- S ::= c | lEr | uS | bSS | "any"   simple expression
-- E ::= SE | S/S | S_S | S^S | S_S^S expression (fraction, sub-, super-, subsuperscript)

--------------------------------------------------------------------------------
-- LEXICAL STRUCTURE

-- TOKENS

-- | "Constants"
data C = Ident String | Numb Double | Const String

-- | "Unary Operators"
data U = UnOp String

-- | "Binary Operators"
data B = BinOp String

-- | "Left Parentheses"
data L = LParen String

-- | "Right Parentheses"
data R = RParen String

-- STRUCTURE

-- | "Expressions"
data E = Single C | Group L E R | Unary U E | Binary B E E | Str String

--------------------------------------------------------------------------------
-- LEXER

-- | Consumes any space in front
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

-- | Parses a symbol then consumes any space after
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- | Parses a float and gives a Double back
number :: Parser Double
number = L.float

-- | Parses a integer and gives a Integer back
integer :: Parser Integer
integer = L.integer

-- | Parses any identifier-like string
identifier :: Parser String
identifier = do
    a <- letterChar
    s <- many (alphaNumChar <|> char '_')
    return $ a:s

-- | Parses a valid Left Parentheses
l :: Parser String
l = choice (map symbol ["(:", "{:", "(", "[", "{"])

-- | Parses a valid Right Parentheses
r :: Parser String
r = choice (map symbol [":)", ":}", ")", "]", "}"])

--------------------------------------------------------------------------------
-- PARSER

-- INDIVIDUALLY

-- | Parses an identifier into the token data type
parseIdent :: Parser C
parseIdent = do
    s <- identifier
    return $ Ident s

-- | Parses a number into the token data type (converts to Double)
parseNumb :: Parser C
parseNumb = do
    d <- try number <|> (integer >>= (\a -> return (fromIntegral a))) <?> "number"
    return $ Numb d

-- | Parses a defined constant into the token data type
parseConst :: Parser C
parseConst = do
    s <- choice (map symbol (sort constantSymbols))
    return $ Const s

-- | Parses a left parentheses into the token data type
parseLParen :: Parser L
parseLParen = do
    s <- l
    return $ LParen s

-- | Parses a right parentheses into the token data type
parseRParen :: Parser R
parseRParen = do
    s <- r
    return $ RParen s

-- | Parse a singleton
parseSingle :: Parser E
parseSingle = do
    c <- parseC
    return $ Single c

-- | Parse a Grouped Expression
parseGroup :: Parser E
parseGroup = do
    lp <- parseL
    e <- parseE
    rp <- parseR
    return $ Group lp e rp

-- | Parses a String Literal
parseStr :: Parser E
parseStr = do
    s <- between (char '"') (char '"') (many anyChar) <?> "string"
    return $ Str s

-- TOGETHER

parseC :: Parser C
parseC = parseConst <|> parseIdent <|> parseNumb <?> "constant term"

parseL :: Parser L
parseL = parseLParen <?> "opening bracket"

parseR :: Parser R
parseR = parseRParen <?> "closing bracket"

parseTerm :: Parser E
parseTerm = parseGroup <|> parseSingle <?> "term"

parseE :: Parser E
parseE = makeExprParser parseTerm operatorTable -- <|> parseStr -- TODO: add support for strings later if necessary

-- OVERALL

parseExpr :: Parser E
parseExpr = do
    many spaceChar
    e <- parseE
    many spaceChar
    eof
    return $ e

--------------------------------------------------------------------------------
-- RESERVED SYMBOLS

constants = [
    ("pi", Measure pi unitless)]

constantSymbols = map fst constants

prefix :: String -> Operator Parser E
prefix s = Prefix (Unary (UnOp s) <$ symbol s)

infixL :: String -> Operator Parser E
infixL s = InfixL (Binary (BinOp s) <$ symbol s)

infixN :: String -> Operator Parser E
infixN s = InfixN (Binary (BinOp s) <$ symbol s)

infixR :: String -> Operator Parser E
infixR s = InfixR (Binary (BinOp s) <$ symbol s)

operatorTable = [
    [infixR "^"],
    [prefix "-"],
    [infixL "*", infixL "/"],
    [infixL "+", infixL "-"]]

--------------------------------------------------------------------------------
-- TRANSLATOR

-- data C = Ident String | Numb Double | Const String
-- data U = UnOp String
-- data B = BinOp String
-- data L = LParen String
-- data R = RParen String
-- data E = Single C | Group L E R | Unary U E | Binary B E E | Str String

-- translate :: E -> Guard e
-- translate (Single c) = translateC c
-- 
-- translateC :: C -> Guard e
-- translateC (Ident s) = Success $ Variable s
-- translateC (Numb n) = Success $ Measure n unitless
-- translateC (Const s) = undefined

--------------------------------------------------------------------------------
-- DEBUGGING

instance Show C where
    show (Ident s) = "[C: " ++ s ++ "]"
    show (Numb d) = "[C: " ++ show d ++ "]"
    show (Const s) = "[C: " ++ s ++ "]"

instance Show U where
    show (UnOp s) = "[U: " ++ s ++ "]"

instance Show B where
    show (BinOp s) = "[B: " ++ s ++ "]"

instance Show L where
    show (LParen s) = "[L: " ++ s ++ "]"

instance Show R where
    show (RParen s) = "[R: " ++ s ++ "]"

instance Show E where
    show (Single c) = "[E: " ++ show c ++ "]"
    show (Group l e r) = "[E: " ++ show l ++ " : " ++ show e ++ " : " ++ show r ++ "]"
    show (Unary f a) = "[E: " ++ show f ++ " : " ++ show a ++ "]"
    show (Binary f a b) = "[E: " ++ show f ++ " : " ++ show a ++ " : " ++ show b ++ "]"
    show (Str s) = "[E: " ++ s ++ "]"
