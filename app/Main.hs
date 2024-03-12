{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LIL-4-1-compressor-leo.dumont
-- File description:
-- Main
-}

module Main (main) where

import ConfigLib
import TypeLib
import FileLib
import System.Environment(getArgs)

{-  | main function

    Main function of the program
-}
main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    validateConf option
