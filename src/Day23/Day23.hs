module Day23.Day23 (solve) where

import           Data.Function        (on)
import           Data.List            (maximumBy, (\\))
import           Data.List.Extra      (groupSort)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           ParserUtils          (Parser)
import           Text.Megaparsec      (choice, errorBundlePretty, many, parse,
                                       sepBy)
import           Text.Megaparsec.Char (char, newline)
import           Utils.Maze

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

isEnd :: Grid -> Point -> Bool
isEnd m p = p == (height m - 1, width m - 2)

pruneMatching :: [a -> Bool] -> [a] -> [a]
pruneMatching fs = filter (not . anyMatch fs)
  where
    anyMatch ffs x = any (\f -> f x) ffs

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
    _   -> error "Unexpected tile encountered"
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

asMap :: Maze Char -> M.Map Point [Point]
asMap m = let
  ps = pruneMatching [isWall] (allPoints m)
  isWall = testPoint m (=='#')
  neighborsPerPoint = map (pruneMatching [isWall] . neighbors4 m) ps
  in M.fromList (zip ps neighborsPerPoint)

findNextIntersection :: M.Map Point [Point] -> Point -> Point -> (Point, Int)
findNextIntersection m prev curr = let
  ns = (m M.! curr) \\ [prev]
  addOne (p, c) = (p, c + 1)
  in case length ns of
    0 -> (curr, 1)
    1 -> addOne $ findNextIntersection m curr (head ns)
    2 -> (curr, 1)
    3 -> (curr, 1)
    _ -> error "Unexpected length found"

onlyIntersections :: M.Map Point [Point] -> [Point]
onlyIntersections m = let
  ns = M.keys m
  isIntersection n = length (m M.! n) `elem` [1, 3, 4]
  in filter isIntersection ns

flatten :: (a, [b]) -> [(a, b)]
flatten (a, bs) = map (a,) bs

unflatten :: Ord a => [(a, b)] -> [(a, [b])]
unflatten = groupSort

simplifyGraph :: M.Map Point [Point] -> M.Map Point [(Point, Int)]
simplifyGraph m = let
  nodes = onlyIntersections m
  edges = concatMap (flatten . (\n -> (n, m M.! n))) nodes :: [(Point, Point)]
  simplifyEdge (curr, next) = (curr, findNextIntersection m curr next)
  xxx = unflatten $ map simplifyEdge edges :: [(Point, [(Point, Int)])]
  in M.fromList xxx

longestPath :: M.Map Point [(Point, Int)] -> (Point -> Bool) -> [(Point, Int)] -> Point -> Int
longestPath m atEnd steps p
  | atEnd p = sum $ map snd steps
  | otherwise = let
    ns = filter (\(pp, _) -> pp `notElem` map fst steps) (m M.! p)
    pathLens = map (\(pp, c) -> longestPath m atEnd ((pp, c):steps) pp) ns
    in maximum (0:pathLens)

part2 :: Input -> IO ()
part2 m = do
  putStr "Part 2: "
  let startPoint = (0, 1)
  let atEnd p = (endPoint m) == p
  let mm = setPointsMatching m (/='#') '.'
  let g = simplifyGraph $ asMap mm
  print $ longestPath g atEnd [((0, 1), 0)] startPoint

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
