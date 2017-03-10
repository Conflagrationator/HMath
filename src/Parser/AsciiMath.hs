-- Parsing AsciiMath
--------------------------------------------------------------------------------

module Parser.AsciiMath where

--------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number
import Value.Vector3D
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Powerable
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

-- | "Simple Expressions"
data S = Single C | Group L E R | Unary U S | Binary B S S | Str String

-- | "Complex Expressions"
data E = Simple S E | Frac S S | Sub S S | Sup S S | SubSup S S S

--------------------------------------------------------------------------------
-- LEXER

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
    s <- many alphaNumChar
    return $ a:s

-- | Parses a valid Left Parentheses
l :: Parser String
l = choice (map string ["(:", "{:", "(", "[", "{"]) <?> "opening bracket"

-- | Parses a valid Right Parentheses
r :: Parser String
r = choice (map string [":)", ":}", ")", "]", "}"]) <?> "closing bracket"

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
    s <- choice (map string (sort constants))
    return $ Const s

-- | Parses a unary operator into the token data type
parseUnOp :: Parser U
parseUnOp = do
    s <- choice (map string (sort unaryOperators))
    return $ UnOp s

-- | Parses a binary operator into the token data type
parseBinOp :: Parser B
parseBinOp = do
    s <- choice (map string (sort binaryOperators))
    return $ BinOp s

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
parseSingle :: Parser S
parseSingle = do
    c <- parseC
    return $ Single c

-- | Parse a Grouped Expression
parseGroup :: Parser S
parseGroup = do
    lp <- parseL
    e <- parseE
    rp <- parseR
    return $ Group lp e rp

-- | Parses a Unary Expression
parseUnary :: Parser S
parseUnary = do
    f <- parseU
    s <- parseS
    return $ Unary f s

-- | Parses a prefix Binary Expression
parsePrefixBinary :: Parser S
parsePrefixBinary = do
    f <- parseB
    a <- parseS
    b <- parseS
    return $ Binary f a b

-- | Parses an infix Binary Expression
parseInfixBinary :: Parser S -- FIXME: left recursive
parseInfixBinary = do
    a <- parseS
    f <- parseB
    b <- parseS
    return $ Binary f a b

-- | Parses a String Literal
parseStr :: Parser S
parseStr = do
    s <- between (char '"') (char '"') (many anyChar) <?> "string"
    return $ Str s

-- | Parses a Simple Expression
parseSimple :: Parser E
parseSimple = do
    s <- parseS
    e <- parseE
    return $ Simple s e

-- | Parses a Fraction
parseFrac :: Parser E
parseFrac = do
    a <- parseS
    char '/'
    b <- parseS
    return $ Frac a b

-- | Parses a Subscript Expression
parseSub :: Parser E
parseSub = do
    a <- parseS
    char '_'
    b <- parseS
    return $ Sub a b

-- | Parses a Superscript Expression
parseSup :: Parser E
parseSup = do
    a <- parseS
    char '^'
    b <- parseS
    return $ Sup a b

-- | Parses a Subscript & Superscript Expression
parseSubSup :: Parser E
parseSubSup = do
    a <- parseS
    char '_'
    b <- parseS
    char '^'
    c <- parseS
    return $ SubSup a b c

-- TOGETHER

parseC :: Parser C
parseC = parseConst <|> parseIdent <|> parseNumb <?> "constant term"

parseU :: Parser U
parseU = parseUnOp <?> "unary operator"

parseB :: Parser B
parseB = parseBinOp <?> "binary operator"

parseL :: Parser L
parseL = parseLParen

parseR :: Parser R
parseR = parseRParen

parseTerm :: Parser S
parseTerm = parseGroup <|> parseSingle

parseS :: Parser S
parseS = makeExprParser parseTerm operatorTable <|> parseStr

parseE :: Parser E
parseE = try parseSimple <|> try parseFrac <|> try parseSub <|> try parseSup <|> parseSubSup -- TODO: check for consumption errors

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
    "pi"]

unaryOperators = [
    "-"]

binaryOperators = [
    "+",
    "*"]

operatorTable = [
    [Prefix (Unary (UnOp "-") <$ string "-")],
    [InfixL (Binary (BinOp "*") <$ string "*"), InfixL (Binary (BinOp "/") <$ string "/")],
    [InfixL (Binary (BinOp "+") <$ string "+"), InfixL (Binary (BinOp "-") <$ string "-")]]

-- TODO: Make unit converter with lists of strings to match and the associated
-- TODO: Double -> Double function that will convert it to SI

-- TODO: Also, include currency as a unit ($) with USD as the SI standard
-- TODO: (for $/kWh etc.)

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

instance Show S where
    show (Single c) = "[S: " ++ show c ++ "]"
    show (Group l e r) = "[S: " ++ show l ++ " : " ++ show e ++ " : " ++ show r ++ "]"
    show (Unary f a) = "[S: " ++ show f ++ " : " ++ show a ++ "]"
    show (Binary f a b) = "[S: " ++ show f ++ " : " ++ show a ++ " : " ++ show b ++ "]"
    show (Str s) = "[S: " ++ s ++ "]"

instance Show E where
    show (Simple s e) = "[E: " ++ show s ++ " : " ++ show e ++ "]"
    show (Frac a b) = "[E: " ++ show a ++ " : " ++ show b ++ "]"
    show (Sub a b) = "[E: " ++ show a ++ " : " ++ show b ++ "]"
    show (Sup a b) = "[E: " ++ show a ++ " : " ++ show b ++ "]"
    show (SubSup a b c) = "[E: " ++ show a ++ " : " ++ show b ++ " : " ++ show c ++ "]"
