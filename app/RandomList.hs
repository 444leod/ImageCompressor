{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- RandomList
-}

module RandomList (
    getRandomList
) where

import System.Random
import IsUnique

type Range = (Int, Int)
type ListSize = Int

{-  | getRandomList function

    This function gets a random list of n numbers within the given range

    Returns a list of random numbers
-}
getRandomList :: ListSize -> Range -> IO [Int]
getRandomList n range = do
    gen <- newStdGen
    let list = take n (randomRs range gen :: [Int])
    case isUnique list of
        True -> return list
        False -> getRandomList n range
