{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LIL-4-1-compressor-leo.dumont
-- File description:
-- Main
-}

module Main (main) where

import Cluster
import Types
import RandomList

getPixel :: Position -> Color -> Pixel
getPixel pos color = (pos, color)

pixelsArray :: [Pixel]
pixelsArray = [
    getPixel (0,0) (33,18,109),     getPixel (1, 2) (1, 2, 3),
    getPixel (2, 3) (4, 5, 6),      getPixel (3, 4) (7, 8, 9),
    getPixel (4, 5) (10, 11, 12),   getPixel (5, 6) (13, 14, 15),
    getPixel (6, 7) (16, 17, 18),   getPixel (7, 8) (19, 20, 21),
    getPixel (8, 9) (22, 23, 24),   getPixel (9, 10) (25, 26, 27)]

main :: IO ()
main = do
    let conf = Conf {k = 3, linesNumber = 10}
    indexes <- getRandomList (k conf) (1, linesNumber conf)
    displayClusters (getClusters pixelsArray (linesNumber conf) indexes)

