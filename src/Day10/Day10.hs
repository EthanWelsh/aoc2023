module Day10.Day10 (solve) where

import           Data.List            (elemIndex, groupBy, nub, sort, (\\))
import           Data.Maybe           (fromJust)
import           ParserUtils          (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline)
import           Utils.Maze

data Tile = Start | Vertical | Horizontal | Ground | NE | NW | SW | SE deriving (Eq)
type Board = Maze Tile
type Input = Board

instance Show Tile where
  show Start      = "S"
  show Vertical   = "|"
  show Horizontal = "-"
  show Ground     = "."
  show NE         = "L"
  show NW         = "J"
  show SW         = "7"
  show SE         = "F"

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

goesToDirs :: Tile -> [Direction]
goesToDirs Vertical   = [North, South]
goesToDirs Horizontal = [East, West]
goesToDirs NE         = [North, East]
goesToDirs NW         = [North, West]
goesToDirs SW         = [South, West]
goesToDirs SE         = [South, East]
goesToDirs t          = error $ "dirsAllowed: Unsupported tile=" ++ show t

acceptsFromDirs :: Tile -> [Direction]
acceptsFromDirs Vertical   = [North, South]
acceptsFromDirs Horizontal = [East, West]
acceptsFromDirs NE         = [South, West]
acceptsFromDirs NW         = [South, East]
acceptsFromDirs SW         = [North, East]
acceptsFromDirs SE         = [North, West]
acceptsFromDirs Ground     = []
acceptsFromDirs t          = error $ "acceptsFrom: Unsupported tile=" ++ show t

isAllowed :: Board -> Point -> Direction -> Bool
isAllowed b p d = inBounds b newPoint && d `elem` goesToDirs oldTile && d `elem` (acceptsFromDirs newTile)
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
    doesAllow = all (\d -> inBounds b (movePoint p d) && d `elem` acceptsFromDirs (getPoint b (movePoint p d)))
    otherDirs dirs = [North, South, East, West] \\ dirs
    doesNotAllowOther dirs = not $ doesAllow (otherDirs dirs)

step :: Board -> Point -> [Point]
step b p = let
  allowedDirs = filter (isAllowed b p) [North, East, South, West]
  in map (movePoint p) allowedDirs

longPaths :: Board -> [Point] -> Point -> [[Point]]
longPaths board visited point = let
    steps = step board point
    filteredSteps = filter (not . (`elem` visited)) steps
    noMoreSteps = null filteredSteps
    newVisited = point : visited
    paths = map (longPaths board newVisited) filteredSteps
    in if noMoreSteps then [visited] else concat paths

furthestPointInLoop :: Board -> Point -> Int
furthestPointInLoop b p = let
  paths = longPaths b [] p
  path1 = (tail . reverse) $ paths !! 0
  path2 = (tail . reverse) $ paths !! 1
  matchIndex = fromJust $ elemIndex True $ zipWith (==) path1 path2
  in 1 + matchIndex

part1 :: Input -> IO ()
part1 board = do
  putStr "Part 1: "
  let startPoint = head $ findPoints board (== Start)
  let startTile = identifyStartTile board startPoint
  let m = setPoint board startPoint startTile
  let d = furthestPointInLoop m startPoint
  print d

pairs :: Show a => [a] -> [(a, a)]
pairs [] = []
pairs [x] = error $ "Expected pairs to be passed a list with an even number of elements but got " ++ (show x)
pairs (x:y:ys) = (x, y) : (pairs ys)

pointsByRow :: [Point] -> [[Point]]
pointsByRow ps = groupBy (\(r1, _) (r2, _) -> r1 == r2) (sort ps)

pointsWithinLoop :: [Point] -> [Point]
pointsWithinLoop ps = concatMap pointsInRowWithinLoop (pointsByRow ps)

pointsInRowWithinLoop :: [Point] -> [Point]
pointsInRowWithinLoop ps = let
  r = fst (head ps)
  cs = map snd ps
  withoutRunsCols = removeMonotonicRuns cs
  withoutRunsPoints = map (\c -> (r, c)) withoutRunsCols
  pointsBetweenPair ((_, c1), (_, c2)) = [(r, c) | c <- [c1..c2]]
  in concatMap pointsBetweenPair (pairs withoutRunsPoints)

countMonotonicRun :: [Int] -> Int
countMonotonicRun [] = 0
countMonotonicRun xs = helper (tail xs) (head xs)
  where
    helper [] _ = 0
    helper (y:ys) prev = if y == prev + 1
                         then 1 + helper ys y
                         else 0

removeMonotonicRuns :: [Int] -> [Int]
removeMonotonicRuns [] = []
removeMonotonicRuns (x:xs) = let
  toSkip = countMonotonicRun (x:xs) - 1
  withSkip = drop toSkip xs
  in x : removeMonotonicRuns withSkip

part2 :: Input -> IO ()
part2 board = do
  putStr "Part 2: "
  let startPoint = head $ findPoints board (== Start)
  let startTile = identifyStartTile board startPoint
  let m = setPoint board startPoint startTile
  let perimeterPoints = (sort . nub . concat) $ longPaths m [] startPoint
  let pointsInLoop = pointsWithinLoop perimeterPoints
  let groundPoints = filter (testPoint board (==Ground)) pointsInLoop
  print $ length groundPoints

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
