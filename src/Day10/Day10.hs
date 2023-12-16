module Day10.Day10 (solve) where

import Control.Monad (void)
import ParserUtils (Parser, integer, charInRange)
import Text.Megaparsec
import Text.Megaparsec.Char (string, char, newline)
import Utils.Maze

-- .....
-- .S-7.
-- .|.|.
-- .L-J.
-- .....

-- | is a vertical pipe connecting north and south.
--- is a horizontal pipe connecting east and west.
--L is a 90-degree bend connecting north and east.
--J is a 90-degree bend connecting north and west.
--7 is a 90-degree bend connecting south and west.
--F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
--S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

data Tile = Start | Vertical | Horizontal | Ground | NE | NW | SW | SE
type Input = Maze Tile

instance Show Tile where
  show Start = "|"
  show Vertical = "-"
  show NE = "L"
  show NW = "J"
  show SW = "7"
  show SE = "F"
  show Ground = "."
  show Start = "S"

parseTile :: Parser Tile
parseTile = choice [ Start <$ char '|'
                   , Vertical <$ char '-'
                   , NE <$ char 'L'
                   , NW <$ char 'J'
                   , SW <$ char '7'
                   , SE <$ char 'F'
                   , Ground <$ char '.'
                   , Start <$ char 'S']

parseLine :: Parser [Tile]
parseLine = many parseTile

parseInput :: Parser Input
parseInput = do
  lines <- parseLine `sepBy` newline
  return $ mazeFromList lines

part1 :: Input -> IO ()
part1 input = do
  putStr $ "Part 1: "
  putStrLn ""
  print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
