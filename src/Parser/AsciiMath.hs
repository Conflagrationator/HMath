-- PARSING AsciiMath
--------------------------------------------------------------------------------

module Parser.AsciiMath where

--------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

--------------------------------------------------------------------------------

integer :: Parser Integer
integer = L.integer

float :: Parser Double
float = L.float

number :: Parser Number
number = do
    n <- try float <|> (fmap fromIntegral integer) <?> "number"
    return $ Measure n unitless















{-
--------------------------------------------------------------------------------
-- LEXICAL STRUCTURE

-- ?

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

--------------------------------------------------------------------------------
-- EXPRESSION PARSER

infixL  name f = InfixL  (f <$ symbol name)
infixR  name f = InfixR  (f <$ symbol name)
infixN  name f = InfixN  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operators = [
    --[infixN "_" Function],
    --[infixN "^" Function],
    --[prefix "-" Function],
    [infixL "+" Function, infixL "-" Function]]

equalities = [
    ]
-}
