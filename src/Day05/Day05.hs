module Day05.Day05 (solve) where

import Control.Monad (void)
import ParserUtils (Parser, integer)
import qualified Data.Set as Set
import Data.Function (on)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import Data.List (find)
import qualified Data.IntervalSet as IntervalSet
import Data.Interval

data Range = Range { destination :: Int
                   , source :: Int
                   , len :: Int
                   } deriving (Show, Eq)
instance Ord Range where
  compare = compare `on` source

type ResourceMap = Set.Set Range
data Almanac = Almanac [Int] [ResourceMap] deriving (Show)

parseRange :: Parser Range
parseRange = do
  d <- integer
  s <- integer
  l <- integer
  return (Range d s l)

parseResourceMap :: Parser (Set.Set Range)
parseResourceMap = do
  ranges <- some parseRange
  return $ Set.fromList ranges

parseInput :: Parser Almanac
parseInput = do
  void $ string "seeds: "
  seeds <- some integer
  void $ string "seed-to-soil map:\n"
  seedSoil <- parseResourceMap
  void $ string "soil-to-fertilizer map:\n"
  soilFertilizer <- parseResourceMap
  void $ string "fertilizer-to-water map:\n"
  fertilizerWater <- parseResourceMap
  void $ string "water-to-light map:\n"
  waterLight <- parseResourceMap
  void $ string "light-to-temperature map:\n"
  lightTemp <- parseResourceMap
  void $ string "temperature-to-humidity map:\n"
  tempHumidity <- parseResourceMap
  void $ string "humidity-to-location map:\n"
  humidityLocation <- parseResourceMap
  return (Almanac seeds [seedSoil, soilFertilizer, fertilizerWater, waterLight,
                         lightTemp, tempHumidity, humidityLocation])

inRange :: Range -> Int -> Bool
inRange (Range _ s l) t = t >= s && t < (s + l)

mapFromRange :: Range -> Int -> Maybe Int
mapFromRange r@(Range d s _) t = if inRange r t
  then Just (d + (t - s))
  else Nothing

mapFromResourceMap :: ResourceMap -> Int -> Int
mapFromResourceMap rs t = fromMaybe t mpd
  where
    mpd = do
      lower <- Set.lookupLE (Range 0 t 0) rs
      mapped <- mapFromRange lower t
      return mapped

mapFromResourceMaps :: [ResourceMap] -> Int -> Int
mapFromResourceMaps maps t = foldl (\i m -> mapFromResourceMap m i) t maps

pairs :: [a] -> [(a, a)]
pairs (a:b:xs) = (a, b) : (pairs xs)
pairs [_] = error "expected multiples of two"
pairs [] = []

createInterval :: (Int, Int) -> Interval Int
createInterval (s, l) = (start <=..< end)
  where
    start = Finite s
    end = Finite (s + l)

seedsAsRanges :: [Int] -> IntervalSet.IntervalSet Int
seedsAsRanges seeds = let
  ps = pairs seeds :: [(Int, Int)]
  intervals = map createInterval ps :: [Interval Int]
  in IntervalSet.fromList $ intervals

reverseRange :: Range -> Range
reverseRange (Range d s l) = (Range s d l)

reverseResourceMap :: ResourceMap -> ResourceMap
reverseResourceMap ranges = let
  l = Set.toList ranges :: [Range]
  r = map reverseRange l
  in Set.fromList r

reverseResourceMaps :: [ResourceMap] -> [ResourceMap]
reverseResourceMaps maps = reverse $ map reverseResourceMap maps

part1 :: Almanac -> IO ()
part1 (Almanac seeds maps) = do
  let seedLocations = map (mapFromResourceMaps maps) seeds
  let minLocation = minimum seedLocations
  putStrLn ( "Part 1: " ++ show minLocation)

part2 :: Almanac -> IO ()
part2 (Almanac seeds maps) = do
  let revd = reverseResourceMaps maps
  let seedRanges = seedsAsRanges seeds
  let ascending = [0..] :: [Int]
  let locationToSeeds = zip ascending $ map (mapFromResourceMaps revd) ascending
  let lowest = find (\(_, s) -> IntervalSet.member s seedRanges) locationToSeeds
  putStrLn $ "Part 2: " ++ show ((fst . fromJust) lowest)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right almanac -> do
            part1 almanac
            part2 almanac
