module Main (main) where

import qualified Day00.Test as Test
import qualified Day01.Day01 as Day01
import qualified Day02.Day02 as Day02
import qualified Day03.Day03 as Day03
import qualified Day04.Day04 as Day04
import qualified Day05.Day05 as Day05
import qualified Day06.Day06 as Day06
import qualified Day07.Day07 as Day07
import System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers =
    [ Test.solve
    , Day01.solve
    , Day02.solve
    , Day03.solve
    , Day04.solve
    , Day05.solve
    , Day06.solve
    , Day07.solve
    ]

main :: IO ()
main = do
    [day, filePath] <- getArgs
    let solver = solvers !! (read day)
    putStrLn ""
    solver filePath
