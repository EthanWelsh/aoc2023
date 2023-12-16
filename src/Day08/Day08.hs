module Day08.Day08 (solve) where

import Control.Monad (void)
import ParserUtils (Parser, integer, charInRange)
import Text.Megaparsec
import Text.Megaparsec.Char (string, char, newline)
import Data.HashMap

data Direction = L | R deriving  (Show, Eq)
type Room = String
type Maze = Map Room (Room, Room)
data Input = Input { dirs :: [Direction], maze :: Maze } deriving (Show)

start :: Room
start = "AAA"

end :: Room
end = "ZZZ"

directionParser :: Parser Direction
directionParser = choice [R <$ char 'R', L <$ char 'L']

roomParser :: Parser Room
roomParser = count 3 (charInRange 'A' 'Z')

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
  lines <- lineParser `sepBy` newline
  let m = fromList lines
  return $ (Input dirs m)

step :: Maze -> Room -> Direction -> Room
step m r L = fst (m ! r)
step m r R = snd (m ! r)

part1 :: Input -> IO ()
part1 input = do
  putStr $ "Part 1: "
  print input

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
