module Day03.Day03 (solve) where

import Data.Char (isDigit)
import Data.List (nub)

type Board = [[Char]]

type Point = (Int, Int)

getPoint :: Board -> Point -> Char
getPoint board (r, c) = board !! r !! c

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && not (c == '.')

neighbours :: Board -> Point -> [Point]
neighbours board (r, c) = filter (inBounds board) [(r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c), (r - 1, c - 1), (r - 1, c + 1), (r + 1, c + 1), (r + 1, c - 1)]

hasAdjacentSymbol :: Board -> Point -> Bool
hasAdjacentSymbol board point =
  let ns = neighbours board point
      cs = map (getPoint board) ns
   in any (isSymbol) cs

height :: Board -> Int
height board = length board

width :: Board -> Int
width board = length (board !! 0)

allPoints :: Board -> [Point]
allPoints board = [(r, c) | r <- [0 .. (height board - 1)], c <- [0 .. (width board - 1)]]

inBounds :: Board -> Point -> Bool
inBounds board (r, c) = r >= 0 && r < (height board) && c >= 0 && c < (width board)

left :: Point -> Point
left (r, c) = (r, c - 1)

findLeftStart :: Board -> Point -> Point
findLeftStart board point = if isPointStart then point else findLeftStart board l
  where
    l = left point
    isOutOfBounds = not (inBounds board l)
    hasNoMoreDigits = not $ isDigit (getPoint board l)
    isPointStart = isOutOfBounds || hasNoMoreDigits

testPoint :: Board -> (Char -> Bool) -> Point -> Bool
testPoint board f p = f (getPoint board p)

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
  let ns = neighbours board point
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
  let board = lines contents
  part1 board
  part2 board

part1 :: [[Char]] -> IO ()
part1 board = do
  let digitPoints = allDigitPoints board
  let nextToSymbols = filter (hasAdjacentSymbol board) digitPoints
  let origins = nub $ map (findLeftStart board) nextToSymbols
  let nums = map (getNumberFromOrigin board) origins
  putStrLn $ "Part 1:" ++ (show (sum nums))

part2 :: [[Char]] -> IO ()
part2 board = do
  let gearPoints = filter (testPoint board (== '*')) (allPoints board)
  let gearRatios = map (gearRatio board) gearPoints
  putStrLn $ "Part 1:" ++ (show (sum gearRatios))
