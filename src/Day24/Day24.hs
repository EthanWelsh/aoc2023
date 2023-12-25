module Day24.Day24 (solve) where

import           Text.Megaparsec
import Control.Monad (void)
import Text.Megaparsec.Char (string, char, newline)
import ParserUtils
import qualified Text.Megaparsec.Char.Lexer as L

newtype Position = Position (Int, Int, Int) deriving (Show, Eq, Ord)
newtype Velocity = Velocity (Int, Int, Int) deriving (Show, Eq, Ord)
data Hail = Hail Position Velocity deriving (Show, Eq, Ord)
type Input = [Hail]

--20, 19, 15 @ 1, -5, -3

negativeInteger :: Parser Int
negativeInteger = do
  void $ char '-'
  n <- integer
  return (n * (-1))

signedInteger :: Parser Int
signedInteger = negativeInteger <|> integer

manySpaces :: Parser ()
manySpaces = void $ many (char ' ')

commaAndSpaces :: Parser ()
commaAndSpaces = do
  void $ char ','
  manySpaces

parseXyz :: Parser (Int, Int, Int)
parseXyz = do
  x <- signedInteger
  commaAndSpaces
  y <- signedInteger
  commaAndSpaces
  z <- signedInteger
  return (x, y, z)

parseLine :: Parser Hail
parseLine = do
  p <- parseXyz
  manySpaces
  void $ string "@"
  manySpaces
  v <- parseXyz
  return (Hail (Position p) (Velocity v))

parseInput :: Parser Input
parseInput = many parseLine

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
