module Parsers.ParserElements
-- Basic parsers
( spaces
, lexstr
, identifier

-- Numeric parsers
, integer
, float

-- Separator parsers
, dot
, colon
, comma
, semicol

-- Group parsers
, parens
, braces
, brackets
, angles
, quotes
) where

------------------------------------------------------------

import qualified Text.Parsec       as P
import qualified Text.Parsec.Token as T
import           Text.Parsec.Language (haskellDef)

------------------------------------------------------------

-- Using Haskell token definitions
lexer      = T.makeTokenParser haskellDef

-- Basic parsers
spaces     = T.whiteSpace lexer
lexstr     = T.lexeme lexer . P.string
identifier = T.identifier lexer

-- Numeric parsers
integer    = T.integer lexer
float      = do
  sign <- P.option id $ do
    lexstr "-"
    return negate
  t <- P.optionMaybe (P.try $ T.float lexer)
  case t of
    Just t -> return (sign t)
    Nothing -> fmap (sign.fromInteger) integer

-- Separator parsers
dot        = T.dot   lexer -- .
colon      = T.colon lexer -- :
comma      = T.comma lexer -- ,
semicol    = T.semi  lexer -- ;

-- Group parsers
parens     = T.parens   lexer -- ()
braces     = T.braces   lexer -- {}
brackets   = T.brackets lexer -- []
angles     = T.angles   lexer -- <>
quotes     = P.between  (lexstr "\"") (lexstr "\"")

