module Main (main) where

-- import Distance
-- import ListToColors
import LineParser
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print (parseLines args)


