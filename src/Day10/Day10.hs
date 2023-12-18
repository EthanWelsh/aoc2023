module Day10.Day10 (solve) where

import ParserUtils (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Utils.Maze
import Data.Maybe (fromJust)
import Data.List (findIndex, (\\))
import qualified Control.Monad as M

data Tile = Start | Vertical | Horizontal | Ground | NE | NW | SW | SE deriving (Eq)
data Direction = North | South | East | West deriving (Show, Eq)
type Board = Maze Tile
type Input = Board

instance Show Tile where
  show Start = "S"
  show Vertical = "|"
  show Horizontal = "-"
  show Ground = "."
  show NE = "L"
  show NW = "J"
  show SW = "7"
  show SE = "F"

parseTile :: Parser Tile
parseTile = choice [ Start <$ char 'S'
                   , Vertical <$ char '|'
                   , Horizontal <$ char '-'
                   , Ground <$ char '.'
                   , NE <$ char 'L'
                   , NW <$ char 'J'
                   , SW <$ char '7'
                   , SE <$ char 'F']

parseLine :: Parser [Tile]
parseLine = many parseTile

parseInput :: Parser Input
parseInput = do
  ls <- parseLine `sepBy` newline
  return $ mazeFromList ls

movePoint :: Point -> Direction -> Point
movePoint p North = north p
movePoint p East = east p
movePoint p South = south p
movePoint p West = west p

goesToDirs :: Tile -> [Direction]
goesToDirs Vertical = [North, South]
goesToDirs Horizontal = [East, West]
goesToDirs NE = [North, East]
goesToDirs NW = [North, West]
goesToDirs SW = [South, West]
goesToDirs SE = [South, East]
goesToDirs t = error $ "dirsAllowed: Unsupported tile=" ++ show t

acceptsFromDirs :: Tile -> [Direction]
acceptsFromDirs Vertical = [North, South]
acceptsFromDirs Horizontal = [East, West]
acceptsFromDirs NE = [South, West]
acceptsFromDirs NW = [South, East]
acceptsFromDirs SW = [North, East]
acceptsFromDirs SE = [North, West]
acceptsFromDirs Ground = []
acceptsFromDirs t = error $ "acceptsFrom: Unsupported tile=" ++ show t

isAllowed :: Board -> Point -> Direction -> Bool
isAllowed b p d = (inBounds b newPoint) && d `elem` (goesToDirs oldTile) && d `elem` (acceptsFromDirs newTile)
  where
    oldTile = getPoint b p
    newPoint = movePoint p d
    newTile = getPoint b newPoint

identifyStartTile :: Board -> Point -> Tile
identifyStartTile b p
  |  onlyAllows [North, South] = Vertical
  |  onlyAllows [East, West] = Horizontal
  |  onlyAllows [North, East] = NE
  |  onlyAllows [North, West] = NW
  |  onlyAllows [South, West] = SW
  |  onlyAllows [South, East] = SE
  |  otherwise = error "Couldn't figure out starting tile"
  where
    onlyAllows dirs = doesAllow dirs && doesNotAllowOther dirs
    doesAllow dirs = all (\d -> d `elem` (acceptsFromDirs (getPoint b (movePoint p d)))) dirs
    otherDirs dirs = [North, South, East, West] \\ dirs
    doesNotAllowOther dirs = not $ doesAllow (otherDirs dirs)

step :: Board -> Point -> [Point]
step b p = let
  allowedDirs = filter (isAllowed b p) [North, East, South, West]
  in map (movePoint p) allowedDirs

longPaths :: Board -> [Point] -> Point -> IO ([[Point]])
longPaths board visited point = do
    let steps = step board point
    let filteredSteps = filter (not . (`elem` visited)) steps
    let noMoreSteps = null filteredSteps
    let newVisited = point : visited
    paths <- M.mapM (longPaths board newVisited) filteredSteps
    let ret = if noMoreSteps then [visited] else concat paths
    return ret

furthestPointInLoop :: Board -> Point -> IO (Int)
furthestPointInLoop b p = do
  paths <- longPaths b [] p
  let path1 = (tail . reverse) $ paths !! 0
  let path2 = (tail . reverse) $ paths !! 1
  let zipped = findIndex (==True) $ zipWith (==) path1 path2
  let ret = fromJust zipped
  return $ ret + 1

part1 :: Input -> IO ()
part1 board = do
  putStr $ "Part 1: "
  let startPoint = head $ findPoints board (== Start)
  let startTile = identifyStartTile board startPoint
  let m = setPoint board startPoint startTile
  d <- furthestPointInLoop m startPoint
  print d

part2 :: Input -> IO ()
part2 board = do
  putStr "Part 2: "
  putStrLn ""

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
