{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- isUnique
-}

module IsUnique (isUnique) where

import Data.List (nub)

{-  | isUnique function

    This function checks if the list is only made of unique elements

    Returns True if the list is only made of unique elements, False otherwise

    isUnique [1, 2, 3] = True
    isUnique [1, 2, 3, 1] = False
    isUnique [] = True
-}
isUnique :: Eq a => [a] -> Bool
isUnique xs = length xs == length (nub xs)
