module Day01.Day01 (solve) where

import Data.List (findIndex, isPrefixOf)

calibrationValue :: [String] -> (String -> Int) -> String -> Int
calibrationValue ns toNum s = let
  left = toNum $ getFirst s ns
  right = toNum $ getLast s ns
  in left * 10 + right

getFirst :: String -> [String] -> String
getFirst = firstPrefix

getLast :: String -> [String] -> String
getLast s ns = reverse $ getFirst (reverse s) (map reverse ns)

firstPrefix :: String -> [String] -> String
firstPrefix s@(_:xs) ps = case findIndex (`isPrefixOf` s) ps of
    Nothing -> firstPrefix xs ps
    (Just i) -> ps !! i
firstPrefix [] _ = error "unexpected"

toNumber :: String -> Int
toNumber "one" = 1
toNumber "two" = 2
toNumber "three" = 3
toNumber "four" = 4
toNumber "five" = 5
toNumber "six" = 6
toNumber "seven" = 7
toNumber "eight" = 8
toNumber "nine" = 9
toNumber d = read d

digits :: [String]
digits = map show ([1..9] :: [Int])

digits_and_words :: [String]
digits_and_words = digits ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  putStrLn $ "Part 1: " ++ show (sum $ map (calibrationValue digits read) $ lines contents)
  putStrLn $ "Part 2: " ++ show (sum $ map (calibrationValue digits_and_words toNumber) $ lines contents)
