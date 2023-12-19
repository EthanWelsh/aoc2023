module Day12.Day12 (solve) where

import           Control.Monad         (void)
import           Data.Function.Memoize
import           Data.List             (intercalate)
import           ParserUtils           (Parser, integer)
import           Text.Megaparsec
import           Text.Megaparsec.Char  (char)

data Puzzle = Puzzle String [Int] deriving (Show)
type Input = [Puzzle]

parsePuzzle :: Parser Puzzle
parsePuzzle = do
  springs <- many (char '#' <|> char '.' <|> char '?')
  void $ char ' '
  clues <- integer `sepBy` char ','
  return $ Puzzle springs clues

parseInput :: Parser Input
parseInput = many parsePuzzle

possible :: String -> [Int] -> Int -> Int
possible [] [] _ = 1
possible [] [r] run = if r == run then 1 else 0
possible [] _ _ = 0
possible xs [] _ = if '#' `notElem` xs then 1 else 0
possible ('#':xs) (r:rs) run
  | (run + 1) > r  = 0
  | (run + 1) == r = memoPossible xs (r:rs) (run + 1)
  | (run + 1) < r  = memoPossible xs (r:rs) (run + 1)
possible ('.':xs) (r:rs) run
  | run == 0 = memoPossible xs (r:rs) 0
  | run == r = memoPossible xs rs 0
  | otherwise = 0
possible ('?':xs) rs run = let
  asOperational = memoPossible ('.':xs) rs run
  asHint = memoPossible ('#':xs) rs run
  in asOperational + asHint
possible xs rs run = error $ "possible: " ++ show xs ++ " " ++ show rs ++ " " ++ show run

memoPossible :: String -> [Int] -> Int -> Int
memoPossible = memoize3 possible

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let poss = map (\(Puzzle xs rs) -> possible xs rs 0) input
  print $ sum poss

unfolded :: Puzzle -> Puzzle
unfolded (Puzzle xs rs) = Puzzle newXs newRs
  where
    newXs = (intercalate "?" . concat . replicate 5) [xs]
    newRs = concat $ replicate 5 rs

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let puzzles = map unfolded input
  let poss = map (\(Puzzle xs rs) -> possible xs rs 0) puzzles
  print $ sum poss

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
