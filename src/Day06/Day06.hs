module Day06.Day06 (solve) where

import Control.Monad (void)
import ParserUtils (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (string)

type Input = [Race]
data Race = Race { time :: Int
                 , distance :: Int
                 } deriving (Show, Eq)

data Action = Action { holdTime :: Int
                     , remainingTime :: Int } deriving (Show)


skipSpaces :: Parser ()
skipSpaces = void $ many (string " ")

parseInput :: Parser Input
parseInput = do
  void $ string "Time:"
  skipSpaces
  times <- many integer
  void $ string "Distance:"
  skipSpaces
  distances <- many integer
  return $ map (\(t, d) -> (Race t d)) (zip times distances)

calculateScore :: Action -> Int
calculateScore a = (remainingTime a) * (holdTime a)

possibleActions :: Race -> [Action]
possibleActions race = map (\ht -> (Action ht (rt - ht))) holdTimes
  where
    rt = time race
    holdTimes = [1..rt]

waysToBeat :: Race -> Int
waysToBeat race = let
  actions = possibleActions race
  scores = map calculateScore actions
  highScore = distance race
  in length $ filter (>highScore) scores

part1 :: Input -> IO ()
part1 input = do
  putStr $ "Part 1: "
  print $ product $ map waysToBeat input

combineDigits :: [Int] -> Int
combineDigits xs = read $ concatMap show xs

oneRace :: [Race] -> Race
oneRace rs = let
  ts = map time rs
  ds = map distance rs
  in Race (combineDigits ts) (combineDigits ds)

part2 :: Input -> IO ()
part2 input = do
  let race = oneRace input
  putStr "Part 2: "
  print $ waysToBeat race

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
