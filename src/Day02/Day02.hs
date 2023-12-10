module Day02.Day02 (solve) where

import ParserUtils (Parser, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string)

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green

type RGB = (Int, Int, Int)

data Game = G Int [RGB] deriving (Show)

number :: Parser Int
number = do
  n <- integer
  return n

rgbParser :: Parser RGB
rgbParser = do
  n <- number
  color <- (string "red" <|> string "green" <|> string "blue")
  case color of
    "red" -> return (n, 0, 0)
    "green" -> return (0, n, 0)
    "blue" -> return (0, 0, n)
    _ -> error "unexpected color"

roundParser :: Parser RGB
roundParser = do
  uncollapsed <- rgbParser `sepBy` (string ", ")
  return $ addRounds uncollapsed

roundsParser :: Parser [RGB]
roundsParser = roundParser `sepBy` (string "; ")

gameParser :: Parser Game
gameParser = do
  _ <- string "Game "
  n <- number
  _ <- string ": "
  rounds <- roundsParser
  return (G n rounds)

inputParser :: Parser [Game]
inputParser = do
  games <- endBy gameParser newline
  eof
  return games

addRounds :: [RGB] -> RGB
addRounds rounds = foldl addRound (0, 0, 0) rounds
  where
    addRound (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

maxRounds :: [RGB] -> RGB
maxRounds rounds = foldl maxRound (0, 0, 0) rounds
  where
    maxRound (a1, a2, a3) (b1, b2, b3) = (max a1 b1, max a2 b2, max a3 b3)

gameIsPossible :: Game -> Bool
gameIsPossible (G _ rounds) =
  let (r, g, b) = maxRounds rounds
   in r <= 12 && g <= 13 && b <= 14

sumOfGameIds :: [Game] -> Int
sumOfGameIds games = foldl addGameId 0 games
  where
    addGameId a (G b _) = a + b

gamePower :: Game -> Int
gamePower (G _ rounds) =
  let (r, g, b) = maxRounds rounds
   in r * g * b

part1 :: [Game] -> IO ()
part1 games = do
  let possibleGames = filter gameIsPossible games
  print $ sumOfGameIds possibleGames

part2 :: [Game] -> IO ()
part2 games = do
  let powerValues = map gamePower games
  print $ sum powerValues

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse inputParser filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right records -> do
              part1 records
              part2 records
  --putStrLn "hello"
  --part1 contents
  --part2 contents
