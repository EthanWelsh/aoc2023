module Day25.Day25 (solve) where

import Text.Megaparsec
import ParserUtils (Parser)
--import Control.Monad (void)
--import Text.Megaparsec.Char (string, char, newline)

type Input = String

parseInput :: Parser Input
parseInput = error "TODO"

part1 :: Input -> IO ()
part1 input = do
  putStr $ "Part 1: "
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
