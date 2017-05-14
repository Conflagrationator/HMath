-- PARSING AsciiMath
--------------------------------------------------------------------------------

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
import Data.List hiding (group) -- FIXME: temporary - outputs for testing

--------------------------------------------------------------------------------
-- LEXICAL STRUCTURE

data Expr = Const String | Unary String Expr | Binary String Expr Expr

instance Show Expr where
    show (Const s) = s
    show (Unary s a) = "(" ++ s ++ ":" ++ show a ++ ")"
    show (Binary s a b) = "(" ++ s ++ ": " ++ show a ++ " " ++ show b ++ ")"

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

group :: Parser a -> Parser a
group p = choice (map (\(l, r) -> try (between (symbol l) (symbol r) p)) [("(", ")"), ("[", "]"), ("{", "}"), ("(:", ":)"), ("{:", ":}")])

--------------------------------------------------------------------------------
-- EXPRESSION

-- | parses a constant
parseConst :: Parser Expr
parseConst = lexeme $ fmap Const (parseNumber <|> parseGreek <|> parseIdent)

-- | parses a segment of the expression
term :: Parser Expr
term = group parseExpression <|> parseConst <?> "term"

-- | parses operators and segments
parseExpression :: Parser Expr
parseExpression = makeExprParser term operatorTable <?> "expression"

operatorTable = [
    [infixN "_"],
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
