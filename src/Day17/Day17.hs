module Day17.Day17 (solve) where

import           Algorithm.Search     (aStar)
import           Data.Char            (digitToInt)
import           Data.List            ((\\))
import           Data.Maybe           (fromJust)
import           Utils.Parsers       (Parser, charInRange)
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char (newline)
import           Utils.Maze

type Grid = Maze Int
data State = State { point     :: Point
                   , direction :: Direction
                   , run       :: Int
                   } deriving (Show, Eq, Ord)

parseLine :: Parser [Int]
parseLine = do
  cs <- many (charInRange '0' '9')
  return $ map digitToInt cs

parseInput :: Parser Grid
parseInput = do
  m <- parseLine `sepBy` newline
  return $ mazeFromList m

endPoint :: Grid -> Point
endPoint g = (height g - 1, width g - 1)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East

allOtherDirs :: Direction -> [Direction]
allOtherDirs d = [North, South, East, West] \\ [d]

outOfBoundsDirs :: Grid -> Point -> [Direction]
outOfBoundsDirs mz p = let
  dirs = [North, South, East, West]
  in filter (not . inBounds mz . movePoint p) dirs

travel :: State -> Direction -> State
travel state newD = let
  newP = movePoint (point state) newD
  newRun = if direction state == newD then run state + 1 else 1
  in State newP newD newRun

neighbors :: Grid -> State -> [State]
neighbors mz state@(State pt dir run) = let
  mustTurn = (run + 1) > 3
  toPrune = concat [ [opposite dir]         -- can't travel opposite direction
                   , outOfBoundsDirs mz pt  -- can't travel out of bounds
                   , [dir | mustTurn]]      -- must turn so can't travel the current dir
  prunedDirs = [North, South, East, West] \\ toPrune
  in map (travel state) prunedDirs

transitionCost :: Grid -> State -> State -> Int
transitionCost m _ state = getPoint m (point state)

heuristic :: Grid -> State -> Int
heuristic maze state = manhattanDistance (point state) (endPoint maze)

isEnd :: Grid -> State -> Bool
isEnd maze state = point state == endPoint maze

search :: Grid -> Point -> Maybe (Int, [State])
search m start = let
  initialState = State start East 0
  in aStar (neighbors m) (transitionCost m) (heuristic m) (isEnd m) initialState

part1 :: Grid -> IO ()
part1 input = do
  putStr "Part 1: "
  let costAndStates = fromJust $ search input (0, 0)
  let cost = fst costAndStates
  print cost

neighborsPt2 :: Grid -> State -> [State]
neighborsPt2 mz state@(State pt dir run) = let
  canTurn = (run + 1) > 4
  mustTurn = canTurn && (run + 1) > 10
  maybeOtherDirs = if not canTurn then allOtherDirs dir else []
  toPrune = concat [ [opposite dir]         -- can't travel the opposite dir
                   , outOfBoundsDirs mz pt  -- can't travel out of bounds
                   , [dir | mustTurn]       -- must turn so can't travel the current dir
                   , maybeOtherDirs]        -- can't turn, eliminate all other dirs
  prunedDirs = [North, South, East, West] \\ toPrune
  in map (travel state) prunedDirs

isEndPt2 :: Grid -> State -> Bool
isEndPt2 maze state = let
  atEndPoint = point state == endPoint maze
  canStop = run state + 1 >= 4
  in atEndPoint && canStop

searchPt2 :: Grid -> Point -> Maybe (Int, [State])
searchPt2 m start = let
  initialState = State start East 0
  in aStar (neighborsPt2 m) (transitionCost m) (heuristic m) (isEndPt2 m) initialState

part2 :: Grid -> IO ()
part2 input = do
  putStr "Part 2: "
  let costAndStates = fromJust $ searchPt2 input (0, 0)
  let cost = fst costAndStates
  print cost

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
