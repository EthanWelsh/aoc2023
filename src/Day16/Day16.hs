module Day16.Day16 (solve) where

import           ParserUtils     (Parser)
import           Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Utils.Maze

type Grid = Maze Char
type Input = Grid

parseLine :: Parser String
parseLine = many (choice [char '.', char '|', char '-', char '\\', char '/'])

parseInput :: Parser Grid
parseInput = do
  ls <- parseLine `sepBy` newline
  return $ mazeFromList ls

nextDirection :: Char -> Direction -> [Direction]
nextDirection '.' d = [d]
nextDirection '|' East = [North, South]
nextDirection '|' West = [North, South]
nextDirection '|' d = [d]
nextDirection '-' North = [East, West]
nextDirection '-' South = [East, West]
nextDirection '-' d = [d]
nextDirection '/' North = [East]
nextDirection '/' East = [North]
nextDirection '/' South = [West]
nextDirection '/' West = [South]
nextDirection '\\' North = [West]
nextDirection '\\' East = [South]
nextDirection '\\' South = [East]
nextDirection '\\' West = [North]

step :: Grid -> (Point, Direction) -> [(Point, Direction)]
step m (p, d) = let
  newDirs = nextDirection (getPoint m p) d
  newPoints = map (\dir -> (movePoint p dir, dir)) newDirs
  in filter (\(pp, _) -> inBounds m pp) newPoints 

startState :: (Point, Direction)
startState = ((0, 0), East)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  --print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print "Hello"

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
