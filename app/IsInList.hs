{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- IsInList
-}

module IsInList (isInList) where

{-  | isInList function

    This function checks if the current number is in the list of numbers

    Returns True if the number is in the list, False otherwise

    isInList 2 [1, 2, 3] = True
    isInList 4 [1, 2, 3] = False
    isInList 1 [] = False
-}
type CurrentNumber = Int
type NumberList = [Int]
isInList :: CurrentNumber -> NumberList -> Bool
isInList _ [] = False
isInList n (x:xs) | n == x = True
                  | otherwise = isInList n xs
