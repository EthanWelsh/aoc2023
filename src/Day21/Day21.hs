module Day21.Day21 (solve) where

import           Data.Function.Memoize
import qualified Data.Set              as Set
import           ParserUtils           (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char  (char, newline)
import           Utils.Maze

type Grid = Maze Char
type Input = Grid
type Points = Set.Set Point

parseInput :: Parser Input
parseInput = do
  ls <- many (char '.' <|> char '#' <|> char 'S') `sepBy` newline
  return $ mazeFromList ls

startPoint :: Grid -> Point
startPoint m = head $ filter (testPoint m (=='S')) (allPoints m)

stepPoint :: Grid -> Point -> Points
stepPoint maze point = memoize (stepHelper maze) point
  where
    stepHelper m p = Set.fromList $ filter (testPoint m (/='#')) (neighbors4 m p)

step :: Grid -> Points -> Points
step m ps = Set.unions $ Set.map (stepPoint m) ps

part1 :: Input -> IO ()
part1 grid = do
  putStr "Part 1: "
  putStrLn ""
  let start = startPoint grid
  let possibilities = iterate (step grid) (Set.singleton start)
  print $ length $ possibilities !! 64

part2 :: Input -> IO ()
part2 grid = do
  putStr "Part 2: "
  let start = startPoint grid
  let possibilities = iterate (step grid) (Set.singleton start)
  print $ length $ possibilities !! 26501365

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
