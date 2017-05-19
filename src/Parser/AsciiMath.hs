-- PARSING AsciiMath
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.AsciiMath where

--------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number
import Value.Variable
import Value.Vector
import Value.Matrix
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Radicalizable
import Constraint.VectorSpace
import Constraint.MeasureSpace
import Constraint.InnerProductSpace

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Data.Maybe
import Data.List -- FIXME: temporary - outputs for testing
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- PARSER
--------------------------------------------------------------------------------
-- parses input string to an expression tree

--------------------------------------------------------------------------------
-- LEXICAL STRUCTURE

data Expr
    = Const String
    | Group String Expr
    | Unary String Expr
    | Binary String Expr Expr
    | Func String (Maybe Expr) (Maybe Expr) Expr

instance Show Expr where
    show (Const s) = s
    show (Group s a) = "[\"" ++ s ++ "\": " ++ show a ++ "]"
    show (Unary name a) = "(" ++ name ++ ":" ++ show a ++ ")"
    show (Binary name a b) = "(" ++ name ++ ": " ++ show a ++ " " ++ show b ++ ")"
    show (Func name sub sup args) = "(" ++ name ++ ": " ++ (if isJust sub then "_(" ++ show (fromJust sub) ++ ") " else "") ++ (if isJust sup then "^(" ++ show (fromJust sup) ++ ") " else "") ++ "(" ++ show args ++ "))"

--------------------------------------------------------------------------------
-- PRIMITIVES

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

--------------------------------------------------------------------------------
-- ROOT

parseNumber :: Parser String
parseNumber = do
    i <- some digitChar
    f <- optional (try (char '.') *> some digitChar)
    return $ if isNothing f then i else i ++ "." ++ fromJust f

parseGreek :: Parser String
parseGreek = choice (map string greekLetters) <?> "greek letter"

parseIdent :: Parser String -- only single letter identifiers for now
parseIdent = fmap (\x -> [x]) letterChar

--------------------------------------------------------------------------------
-- EXPRESSION

-- | parses a constant
parseConst :: Parser Expr
parseConst = lexeme $ fmap Const (parseNumber <|> parseGreek <|> parseIdent)

-- | parses a group encased in brackets of some kind
parseGroup :: Parser Expr
--parseGroup = choice (map (\(l, r) -> try (between (symbol l) (symbol r) parseExpression)) parenPairs)
parseGroup = choice (map (\(l, r) -> try(fmap (Group l) (between (symbol l) (symbol r) parseExpression))) parenPairs)
  where
    parenPairs = [("(", ")"), ("[", "]"), ("{", "}"), ("(:", ":)"), ("{:", ":}")]

-- | parses a known function
parseFunction :: Parser Expr
parseFunction = do
    name <- choice (map symbol functionNames)
    sub <- optional (symbol "_" *> parseTerm)
    sup <- optional (symbol "^" *> parseTerm)
    args <- between (symbol "(") (symbol ")") parseExpression
    return $ Func name sub sup args

-- | parses a segment of the expression
parseTerm :: Parser Expr
parseTerm = try parseFunction <|> parseGroup <|> parseConst <?> "term"

-- | parses operators and segments
parseExpression :: Parser Expr
parseExpression = makeExprParser parseTerm operatorTable <?> "expression"

operatorTable = [
    [infixN "_"],
    [infixL ","],
    [prefix "-"],
    [infixR "^"],
    [infixL "*", implicit "*", infixL "/", infixL "-:", infixL "xx"],
    [infixL "+", infixL "-"]]

prefix  name = Prefix  (Unary  name <$ symbol name)
infixL  name = InfixL  (Binary name <$ symbol name)
infixR  name = InfixR  (Binary name <$ symbol name)
infixN  name = InfixN  (Binary name <$ symbol name)
postfix name = Postfix (Unary  name <$ symbol name)
implicit op  = InfixL  (Binary op   <$ symbol "") -- only use once in table

--------------------------------------------------------------------------------

greekLetters = [
    "alpha",
    "beta",
    "chi",
    "delta",
    "Delta",
    "epsilon",
    "eta",
    "gamma",
    "Gamma",
    "iota",
    "Kappa",
    "lambda",
    "Lambda",
    "mu",
    "nu",
    "omega",
    "Omega",
    "phi",
    "Phi",
    "pi",
    "Pi",
    "psi",
    "Psi",
    "rho",
    "sigma",
    "Sigma",
    "tau",
    "theta",
    "Theta",
    "upsilon",
    "varepsilon",
    "varphi",
    "vartheta",
    "xi",
    "Xi",
    "zeta"]

functionNames = [
    "ln",
    "log"]

--------------------------------------------------------------------------------
-- TRANSLATOR
--------------------------------------------------------------------------------
-- translates parsed expression tree to actual expression structure

-- TODO: make something that translates the tree
-- FIXME: the issue is to make a function that takes an Expr and returns 
--        Maybe(Expression) of any kind of Expression. the type of the Expression
--        cannot be known at compile time
-- TODO: maybe making a wrapper type that encapsulates an expression, then
--       unwrapping it and potentially failing when putting it in a container
--       expression, then finally returning on the outermost layer a wrapper,
--       knowing that ther is some valid expression inside, just not knowing the type of it.

-- NOTE: 1st attempt

--------------------------------------------------------------------------------
-- WRAPPER TYPE

data ExpressionWrapper = forall e r. Expression e r => WrappedExpression e

instance Show ExpressionWrapper where
    show (WrappedExpression e) = show e

wrapExpression :: Expression e r => e -> ExpressionWrapper
wrapExpression = WrappedExpression

--------------------------------------------------------------------------------
-- TRANSLATOR

translateExpression :: Expr -> Maybe ExpressionWrapper
translateExpression (Const s)
    | isJust (translateNumber s) = fmap wrapExpression (translateNumber s)
    | isJust (translateGreek s) = fmap wrapExpression (translateGreek s)
    | isJust (translateIdent s) = fmap wrapExpression (translateIdent s)
    | otherwise = Nothing
-- translateExpression (Group s a)
-- translateExpression (Unary name a)
-- translateExpression (Binary name a b)
--     | name == "+" = fmap wrapExpression (translateAddition (translateExpression a) (translateExpression b))
--     -- TODO: find out where to put checking for name string equality (for use with more complicated ones like log and ln, etc.)
--     | otherwise = Nothing
-- translateExpression (Func name sub sup args)

--------------------------------------------------------------------------------
-- SPECIFIC TRANSLATORS

translateNumber :: String -> Maybe Number
translateNumber s = if isJust (readMaybe s :: Maybe Double) then Just (Measure (fromJust (readMaybe s :: Maybe Double)) unitless) else Nothing -- FIXME: find out a way to read units

translateGreek :: String -> Maybe Variable
translateGreek s = if s `elem` greekLetters then Just (Variable s) else Nothing

translateIdent :: String -> Maybe Variable
translateIdent s = if length s == 1 then Just (Variable s) else Nothing -- TODO: add support for checking if they're defined already. or should that go in the parser?

-- translateAddition :: (Expression a n, Expression b m, Addable n m c) => String -> Maybe ExpressionWrapper -> Maybe ExpressionWrapper -> Maybe (Addition a b c)
-- translateAddition s e1 e2 -- if e1 is type a & e2 is type b where they are also addable to c
--     | s == "+" && isJust e1 && isJust e1 = undefined
--     | otherwise = Nothing

--------------------------------------------------------------------------------
-- WRITER
--------------------------------------------------------------------------------
-- takes an actual expression tree and outputs AsciiMath

class AsciiMathDisplayable e where
    displayAsciiMath :: e -> String

instance AsciiMathDisplayable Number where
    displayAsciiMath (Measure a u) = show a ++ if u == unitless then "" else "text(" ++ show u ++ ")"
    -- FIXME: this doesn't actually deal with the real problem
    -- TODO: need to make it extendable so anyone can write a new writing method for all
    --       expression instances
