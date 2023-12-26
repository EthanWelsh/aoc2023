module Day25.Day25 (solve) where

import           Control.Monad        (void)
import           Data.Function        (on)
import           Data.List            (maximumBy)
import qualified Data.Map             as M
import           ParserUtils
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline, string)
import           Utils.Graph
import           Utils.Search

type Input = M.Map String [String]

parseNode :: Parser String
parseNode = many (charInRange 'a' 'z')

parseLine :: Parser (String, [String])
parseLine = do
  src <- parseNode
  void $ string ": "
  ns <- parseNode `sepBy` (char ' ')
  return (src, ns)

parseInput :: Parser Input
parseInput = do
  ls <- parseLine `sepBy` newline
  return $ M.fromList ls

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs [_]      = []
pairs (x:y:ys) = (x, y) : (pairs (y:ys))

mostUsedEdge :: Ord a => Graph a -> a -> a -> (a, a)
mostUsedEdge g src dst = let
  edgesInAllPaths = concatMap pairs $ allPaths (neighbors g) (==dst) src
  m = foldl (\mm pair -> M.insertWith (+) pair 1 mm) M.empty edgesInAllPaths
  in fst $ maximumBy (compare `on` snd) (M.toList m)

printGraph :: Show a => Graph a -> IO ()
printGraph g = putStrLn $ unlines $ map (\(k, v) -> (show k) ++ ": " ++ (show v) ) (M.toList (graphAsMap g))

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  putStrLn ""
  let src = "ggk"
  let dst = "dkl"
  let g0 = makeBidirectional $ graphFromMap input
  let e1 = mostUsedEdge g0 src dst
  let g1 = removeBidirectionalEdge g0 e1
  let e2 = mostUsedEdge g1 src dst
  let g2 = removeBidirectionalEdge g1 e2
  let e3 = mostUsedEdge g2 src dst
  let g3 = removeBidirectionalEdge g2 e3
  let componentSizes = map length (connectedComponents g3)
  print e1
  print e2
  print e3
  print componentSizes
  print $ product componentSizes

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  --print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
