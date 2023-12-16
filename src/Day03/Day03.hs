module Day03.Day03 (solve) where

import Data.Char (isDigit)
import Data.List (nub)
import Utils.Maze

type Board = Maze Char

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && not (c == '.')

hasAdjacentSymbol :: Board -> Point -> Bool
hasAdjacentSymbol board point =
  let ns = neighbors8 board point
      cs = map (getPoint board) ns
   in any (isSymbol) cs

findLeftStart :: Board -> Point -> Point
findLeftStart board point = if isPointStart then point else findLeftStart board l
  where
    l = west point
    isOutOfBounds = not (inBounds board l)
    hasNoMoreDigits = not $ isDigit (getPoint board l)
    isPointStart = isOutOfBounds || hasNoMoreDigits

allDigitPoints :: Board -> [Point]
allDigitPoints board = filter (testPoint board isDigit) (allPoints board)

allPointsToRight :: Board -> Point -> [Point]
allPointsToRight board (r, c) = [(r, x) | x <- [c .. (width board - 1)]]

getNumberFromOrigin :: Board -> Point -> Int
getNumberFromOrigin board point =
  let numPoints = takeWhile (testPoint board isDigit) (allPointsToRight board point)
   in read (map (getPoint board) numPoints)

getNumbersAroundPoint :: Board -> Point -> [Int]
getNumbersAroundPoint board point =
  let ns = neighbors8 board point
      numPoints = filter (testPoint board isDigit) ns
      origins = nub $ map (findLeftStart board) numPoints
   in map (getNumberFromOrigin board) origins

gearRatio :: Board -> Point -> Int
gearRatio board point = case (getNumbersAroundPoint board point) of
  [a, b] -> a * b
  _ -> 0

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  let board = mazeFromList $ lines contents
  part1 board
  part2 board

part1 :: Board -> IO ()
part1 board = do
  let digitPoints = allDigitPoints board
  let nextToSymbols = filter (hasAdjacentSymbol board) digitPoints
  let origins = nub $ map (findLeftStart board) nextToSymbols
  let nums = map (getNumberFromOrigin board) origins
  putStrLn $ "Part 1:" ++ (show (sum nums))

part2 :: Board -> IO ()
part2 board = do
  let gearPoints = filter (testPoint board (== '*')) (allPoints board)
  let gearRatios = map (gearRatio board) gearPoints
  putStrLn $ "Part 2:" ++ (show (sum gearRatios))
