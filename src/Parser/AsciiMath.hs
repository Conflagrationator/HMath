-- PARSING AsciiMath
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeFamilies #-}
-- OverlappingInstances,

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

import Data.Typeable
import Data.Dynamic

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
    [infixL "*", infixL "/", infixL "-:", infixL "xx", implicit "*"],
    [infixL "+", infixL "-"]]

prefix  name = Prefix  (Unary  name <$ symbol name)
infixL  name = InfixL  (Binary name <$ symbol name)
infixR  name = InfixR  (Binary name <$ symbol name)
infixN  name = InfixN  (Binary name <$ symbol name)
postfix name = Postfix (Unary  name <$ symbol name)
implicit op  = InfixL  (Binary op   <$ symbol "") -- only use once in table & must always be last in list (since it matches an empty string (which always succeeds))

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

--------------------------------------------------------------------------------
-- WRAPPER TYPE

data ExpressionWrapper where
    WrappedExpression :: (Expression e r) => e -> ExpressionWrapper

--------------------------------------------------------------------------------
-- TRANSLATOR

translateExpression :: Expr -> Maybe ExpressionWrapper
translateExpression (Const s)
    | isJust (readMaybe s :: Maybe Double) = Just (WrappedExpression (Measure (fromJust (readMaybe s :: Maybe Double)) unitless)) -- FIXME: find out a way to read units
    | s `elem` greekLetters = Just (WrappedExpression (Variable s))
    | length s == 1 = Just (WrappedExpression (Variable s))
    | otherwise = Nothing
translateExpression (Group s a) = undefined
translateExpression (Unary name a) = undefined
translateExpression (Binary name a b)
    | name == "+" = translateAddition (translateExpression a) (translateExpression b)
    | otherwise = Nothing
translateExpression (Func name sub sup args) = undefined

--------------------------------------------------------------------------------
-- SPECIFIC TRANSLATORS

translateAddition :: Maybe ExpressionWrapper -> Maybe ExpressionWrapper -> Maybe ExpressionWrapper
translateAddition (Just (WrappedExpression a)) (Just (WrappedExpression b)) = undefined
translateAddition _ _ = Nothing



-- !!!! BEGIN GENERALIZED `constructAddition` FUNCTION !!!!

class AreAddable a b c where
    constructAddition :: a -> b -> Maybe (Addition a b c)
    
data HTrue
data HFalse

class AreAddable' flag a b c where
    constructAddition' :: flag -> a -> b -> Maybe (Addition a b c)

instance (AreAddablePred flag a b c, AreAddable' flag a b c) => AreAddable a b c where
    constructAddition = constructAddition' (undefined :: flag)




class AreAddablePred flag a b c | a b -> flag, a b -> c where {}

-- instance {-# OVERLAPPABLE #-} TypeCast flag HFalse => AreAddablePred flag a b c
instance {-# OVERLAPPABLE #-} (flag ~ HFalse) => AreAddablePred flag a b Number -- FIXME: Number should not be here, it's a throwaway type

instance AreAddablePred HTrue Number Number Number

-- type family AreAddablePred a b where
--     AreAddablePred Number Number = HTrue


instance {-# OVERLAPPABLE #-} (Expression a m, Expression b n, Addable m n c) => AreAddable' HTrue a b c where
    constructAddition' _ a b = Just (Addition a b)
instance {-# OVERLAPPABLE #-} AreAddable' HFalse a b c where
    constructAddition' _ a b = Nothing




class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x



-- TESTING
a = Measure 5 unitless
b = Measure 6 unitless
c = Variable "s"
-- d = Vector (a :> b :> Nil)




--------------------------------------------------------------------------------
-- WRITER
--------------------------------------------------------------------------------
-- takes an actual expression tree and outputs AsciiMath

-- class (Expression e r) => AsciiMathDisplayable e where
--     -- TODO: use solution here: https://wiki.haskell.org/GHC/AdvancedOverlap
--     -- to implement testing of whether the expression implements this and can
--     -- be displayed or just try to show it's default show value (or something)
