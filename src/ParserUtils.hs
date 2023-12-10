module ParserUtils (
    Parser,
    sc,
    lexeme,
    integer,
    symbol,
    comma,
    semicolon,
    colon,
    dot,
    pipe,
    parens,
)
where

import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

semicolon :: Parser String
semicolon = symbol ";"

colon :: Parser String
colon = symbol ":"

dot :: Parser String
dot = symbol "."

pipe :: Parser String
pipe = symbol "|"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")