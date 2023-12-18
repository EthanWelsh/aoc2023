module Utils.Maze (
  Point,
  Maze,
  north,
  south,
  east,
  west,
  mazeFromList,
  height,
  width,
  getPoint,
  setPoint,
  findPoints,
  testPoint,
  inBounds,
  neighbors4,
  neighbors8,
  allPoints,
  allPointsSatisfying,
) where

import Control.Lens

type Point = (Int, Int)
data Maze a = Maze [[a]]

instance Show a => Show (Maze a) where
  show (Maze m) = unlines $ map (concatMap show) m

mazeFromList :: [[a]] -> Maze a
mazeFromList m = (Maze m)

north :: Point -> Point
north (r, c) = (r - 1, c)

east :: Point -> Point
east (r, c) = (r, c + 1)

south :: Point -> Point
south (r, c) = (r + 1, c)

west :: Point -> Point
west (r, c) = (r, c - 1)

northEast :: Point -> Point
northEast = north . east

southEast :: Point -> Point
southEast = south . east

southWest :: Point -> Point
southWest = south . west

northWest :: Point -> Point
northWest = north . west

height :: Maze a -> Int
height (Maze m) = length $ m

width :: Maze a -> Int
width (Maze m) = length $ m !! 0

getPoint :: Maze a -> Point -> a
getPoint (Maze m) (r, c) = (m !! r) !! c

replacePoint :: [[a]] -> Point -> a -> [[a]]
replacePoint g (r, c) v = let
    oldRow = g !! r
    newRow = (element c .~ v) oldRow
  in (element r .~ newRow) g

setPoint :: Maze a -> Point -> a -> Maze a
setPoint (Maze m) p v = (Maze (replacePoint m p v))

testPoint :: Maze a -> (a -> Bool) -> Point -> Bool
testPoint m f p = f (getPoint m p)

findPoints :: Maze a -> (a -> Bool) -> [Point]
findPoints m f = filter (testPoint m f) (allPoints m)

inBounds :: Maze a -> Point -> Bool
inBounds m (r, c) = r >= 0 && r < (height m) && c >= 0 && c < (width m)

neighbors4 :: Maze a -> Point -> [Point]
neighbors4 m p = filter (inBounds m) $ map ($ p) dirs
  where dirs = [north, east, south, west]

neighbors8 :: Maze a -> Point -> [Point]
neighbors8 m p = filter (inBounds m) $ map ($ p) dirs
  where dirs = [north, northEast, east, southEast, south, southWest, west, northWest]

allPoints :: Maze a -> [Point]
allPoints m = [(r, c) | r <- [0 .. (height m - 1)], c <- [0 .. (width m - 1)]]

allPointsSatisfying :: Maze a -> (Point -> Bool) -> [Point]
allPointsSatisfying m f = filter f (allPoints m)