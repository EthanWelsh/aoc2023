module Day08.Day08 (solve) where

import           Control.Monad        (void)
import qualified Data.HashMap         as HM
import           Utils.Parsers       (Parser, charInRange)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline, string)

data Direction = L | R deriving  (Show, Eq)
type Room = String
type Maze = HM.Map Room (Room, Room)
data Input = Input { dirs :: [Direction], maze :: Maze } deriving (Show)

directionParser :: Parser Direction
directionParser = choice [R <$ char 'R', L <$ char 'L']

roomParser :: Parser Room
roomParser = count 3 (charInRange 'A' 'Z' <|> charInRange '0' '9')

lineParser :: Parser (Room, (Room, Room))
lineParser = do
  src <- roomParser
  void $ string " = ("
  l <- roomParser
  void $ string ", "
  r <- roomParser
  void $ string ")"
  return (src, (l, r))

parseInput :: Parser Input
parseInput = do
  dirs <- manyTill directionParser newline
  void $ newline
  ls <- lineParser `sepBy` newline
  let m = HM.fromList ls
  return (Input dirs m)

step :: Maze -> Direction -> Room -> Room
step m L r = fst (m HM.! r)
step m R r = snd (m HM.! r)

stepsToEnd :: Maze -> (Room -> Bool) -> [Direction] -> Room -> Int
stepsToEnd _ _ [] _ = error "Directions should be infinite"
stepsToEnd m isEnd (d:ds) r = if isEnd r then 0 else 1 + remainingSteps
  where
    remainingSteps = stepsToEnd m isEnd ds (step m d r)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let ds = (concat . repeat) $ dirs input :: [Direction]
  let m = maze input
  print $ stepsToEnd m (=="ZZZ") ds "AAA"

stepsToEndPt2 :: Maze -> [Room] -> [Direction] -> Int
stepsToEndPt2 m rs ds = foldl1 lcm $ map (stepsToEnd m isEnd ds) rs
  where isEnd room = 'Z' == last room

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let ds = (concat . repeat) $ dirs input :: [Direction]
  let m = maze input
  let starts = filter ((=='A') . last) (HM.keys m)
  print $ stepsToEndPt2 m starts ds

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
