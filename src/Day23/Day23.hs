module Day23.Day23 (solve) where

import qualified Data.Set             as S
import           ParserUtils          (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline, string)
import           Utils.Maze
import Data.List (maximumBy)
import Data.Function (on)


type Grid = Maze Char
type Input = Grid

parseLine :: Parser String
parseLine = many (choice [ char '#'
                         , char '.'
                         , char '^'
                         , char '>'
                         , char 'v'
                         , char '<' ])

parseInput :: Parser Input
parseInput = do
  ls <- parseLine `sepBy` newline
  return $ mazeFromList ls

endPoint :: Grid -> Point
endPoint m = (height m - 1, width m - 2)

containsEnd :: Grid -> S.Set Point -> Bool
containsEnd m visited = endPoint m `S.member` visited

isEnd :: Grid -> Point -> Bool
isEnd m p = p == (height m - 1, width m - 2)

pruneMatching :: [a -> Bool] -> [a] -> [a]
pruneMatching fs = filter (not . anyMatch fs)
  where
    anyMatch fs x = any (\f -> f x) fs

step :: Grid -> S.Set Point -> Point -> [Point]
step m visited p = let
  currentTile = getPoint m p
  isWall = testPoint m (=='#')
  isVisited = (`S.member` visited)
  outOfBounds = not . inBounds m
  ns = case currentTile of
    '.' -> neighbors4 m p
    '^' -> [movePoint p North]
    '>' -> [movePoint p East]
    'v' -> [movePoint p South]
    '<' -> [movePoint p West]
  in pruneMatching [isWall, isVisited, outOfBounds] ns

longestPathPoints :: Grid -> S.Set Point -> Point -> S.Set Point
longestPathPoints m visited p = let
  ns = step m visited p
  newVisited = S.insert p visited
  pathPoints = map (longestPathPoints m newVisited) ns
  longPath = maximumBy (compare `on` S.size) (S.empty : pathPoints)
  in if isEnd m p then newVisited else longPath

part1 :: Input -> IO ()
part1 m = do
  putStr "Part 1: "
  let startPoint = (0, 1)
  let pointsInPath = S.toList $ longestPathPoints m S.empty startPoint
  print $ length pointsInPath - 1

printMaze :: Maze Char -> IO ()
printMaze m = putStrLn $ unlines (mazeToList m)

part2 :: Input -> IO ()
part2 m = do
  putStr "Part 2: "
  let startPoint = (0, 1)
  let mm = setPointsMatching m (/='#') '.'
  let pointsInPath = S.toList $ longestPathPoints mm S.empty startPoint
  print $ length pointsInPath - 1

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
