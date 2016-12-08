-- AsciiMath Parsing Functionality
----------------------------------------------------------------

module AsciiMath.Parsing where

----------------------------------------------------------------

import Text.Parsec

----------------------------------------------------------------
-- ASCIIMATH GRAMMAR REFERENCE
----------------------------------------------------------------

-- c ::= [A-z] | numbers | greek letters | other constant symbols
-- u ::= 'sqrt' | 'text' | 'bb' | ... other unary symbols for font commands
-- b ::= 'frac' | 'root' | 'stackrel' -- binary symbols
-- l ::= ( | [ | { | (: | {:          -- left brackets
-- r ::= ) | ] | } | :) | :}          -- right brackets
-- S ::= c | lEr | uS | bSS | "any"   -- simple expression
-- E ::= SE | S/S | S_S | S^S | S_S^S -- expression (fraction, sub-, super-, subsuperscript)

----------------------------------------------------------------
-- PLACEHOLDER DATA TYPES
----------------------------------------------------------------



----------------------------------------------------------------
-- DEFINITIONS
----------------------------------------------------------------



----------------------------------------------------------------
-- PRIMITIVES
----------------------------------------------------------------



----------------------------------------------------------------
-- GRAMMAR
----------------------------------------------------------------

--c = letter <|> number <|> greek <|> constants
--u =
--b =
--l =
--r =
--S =
--E =
