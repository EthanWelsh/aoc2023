module Day07.Day07 (solve) where

import           Control.Monad        (void)
import           Data.Char            (intToDigit)
import           Data.List            (group, sort, sortBy)
import           ParserUtils          (Parser, integer)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Card = Int
data Hand = Hand [Card] deriving (Eq)
data Game = Game { hand :: Hand
                 , bid  :: Int } deriving (Show, Eq)
type Input = [Game]

data Rank = HighCard Int | OnePair Int | TwoPair Int Int |
            ThreeKind Int | FullHouse Int Int | FourKind Int |
            FiveKind Int deriving (Show, Eq, Ord)

instance Show Hand where
  show (Hand cards) = map toLetter cards

parseCard :: Parser Card
parseCard = choice [ 2 <$ char '2' , 3 <$ char '3', 4 <$ char '4'
                   , 5 <$ char '5', 6 <$ char '6', 7 <$ char '7'
                   , 8 <$ char '8', 9 <$ char '9', 10 <$ char 'T'
                   , 11 <$ char 'J', 12 <$ char 'Q', 13 <$ char 'K'
                   , 14 <$ char 'A']

parseGame :: Parser Game
parseGame = do
  cards <- many parseCard
  void $ char ' '
  bid <- integer
  return (Game (Hand cards) bid)

parseInput :: Parser Input
parseInput = many parseGame

rsort :: Ord a => [a] -> [a]
rsort = reverse . sort

eraseInt :: Rank -> Rank
eraseInt (HighCard _)    = HighCard 2
eraseInt (OnePair _)     = OnePair 2
eraseInt (TwoPair _ _)   = TwoPair 2 2
eraseInt (ThreeKind _)   = ThreeKind 2
eraseInt (FullHouse _ _) = FullHouse 2 2
eraseInt (FourKind _)    = FourKind 2
eraseInt (FiveKind _)    = FiveKind 2

toLetter :: Int -> Char
toLetter x
  | x < 10 = intToDigit x
  | x == 10 = 'T'
  | x == 11 = 'J'
  | x == 12 = 'Q'
  | x == 13 = 'K'
  | x == 14 = 'A'
  | otherwise = error $ concat ["toLetter can't be called on l=", show x]

kinds :: Hand -> [Rank]
kinds (Hand cards) = rsort $ combineRanks $ rsort $ map toRank valAndCount
  where
    groups = (group . sort) cards
    valAndCount = map (\l -> (head l, length l)) groups
    toRank (v, 1) = HighCard v
    toRank (v, 2) = OnePair v
    toRank (v, 3) = ThreeKind v
    toRank (v, 4) = FourKind v
    toRank (v, 5) = FiveKind v
    toRank (v, c) = error $ concat [ "error on call to toRank(v="
                                   , show v, ", c=", show c
                                   , ") c should be one of {5, 4, 3, ...}"]

combineRanks :: [Rank] -> [Rank]
combineRanks (OnePair x : OnePair y : xs)   = TwoPair x y : xs
combineRanks (ThreeKind x : OnePair y : xs) = FullHouse x y : xs
combineRanks xs                             = xs

calculateScore :: Input -> (Hand -> Hand -> Ordering) -> Int
calculateScore input f = let
  games = sortBy (\(Game a _) (Game b _) -> f a b) input
  rankedGames = zip [1..] games :: [(Int, Game)]
  valuePerGame = map (\(rank, Game _ bid) -> rank * bid) rankedGames :: [Int]
  in sum valuePerGame

comparePt1 :: Hand -> Hand -> Ordering
comparePt1 a@(Hand aa) b@(Hand bb) = let
  topRank = maximum . kinds
  topRankWithoutInt = eraseInt . topRank
  in (topRankWithoutInt a, aa) `compare` (topRankWithoutInt b, bb)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ calculateScore input comparePt1

upgradeRank :: Int -> Rank -> Rank
upgradeRank 0 r = r
upgradeRank _ (HighCard 11)    = HighCard 11
upgradeRank 1 (HighCard x)     = OnePair x
upgradeRank 2 (HighCard x)     = ThreeKind x
upgradeRank 3 (HighCard x)     = FourKind x
upgradeRank 4 (HighCard x)     = FiveKind x
upgradeRank 2 (OnePair 11)     = OnePair 11
upgradeRank 1 (OnePair x)      = ThreeKind x
upgradeRank 2 (TwoPair 11 y)   = FourKind y
upgradeRank 2 (TwoPair x 11)   = FourKind x
upgradeRank 1 (TwoPair x y)    = FullHouse x y
upgradeRank 3 (FullHouse 11 y) = FiveKind y
upgradeRank 2 (FullHouse x 11) = FiveKind x
upgradeRank 3 (ThreeKind 11)   = ThreeKind 11
upgradeRank 1 (ThreeKind x)    = FourKind x
upgradeRank 4 (FourKind 11)    = FiveKind 0
upgradeRank 1 (FourKind x)     = FiveKind x
upgradeRank _ (FiveKind 11)    = FiveKind 11
upgradeRank c r = error $ concat ["Couldn't updgrade rank=(", show r, ") jackCount=", show c]

jacksCount :: Hand -> Int
jacksCount (Hand cs) = length $ filter (==11) cs

handRankWithJacks :: Hand -> Rank
handRankWithJacks h = let
  ranksWithoutJacks = kinds h
  ranksWithJacks = map (upgradeRank (jacksCount h)) ranksWithoutJacks
  in maximum ranksWithJacks

replace :: Eq a => a -> a -> [a] -> [a]
replace old new xs = map (\x -> if x == old then new else x) xs

comparePt2 :: Hand -> Hand -> Ordering
comparePt2 a@(Hand aa) b@(Hand bb) = let
  hrank = eraseInt . handRankWithJacks
  withLowJacks = replace 11 0
  in (hrank a, withLowJacks aa) `compare` (hrank b, withLowJacks bb)

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ calculateScore input comparePt2

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
