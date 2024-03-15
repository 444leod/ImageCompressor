{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- GetCluster
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cluster (
    getClusters,
    displayCluster,
    displayClusters
) where

import Types
import IsInList

type Indexes = [Int]
type CurrentIndex = Int

{-  | getClusters function

    This function gets the reference pixels (clusters)
        using a random list of indexes

    Returns a list of reference pixels
-}
getClusters :: [Pixel] -> Int -> Indexes -> [Cluster]
getClusters pix lineNum indexes = getPtsByIndex pix indexes lineNum

{-  | makeCluster function

    This function creates a cluster using the given pixel

    Returns a cluster
-}
makeCluster :: Pixel -> Cluster
makeCluster (pos, color) = Cluster {color = color, pixels = [(pos, color)]}

{-  | getPtsByIndex function

    This function gets the reference pixels (clusters) using a random list of
        indexes
    Add the current pixel to the list of clusters if the index is in the list

    Returns a list of cluster
-}
getPtsByIndex :: [Pixel] -> Indexes -> CurrentIndex -> [Cluster]
getPtsByIndex [] _ _ = []
getPtsByIndex (pixel:xs) pts n
    | isInList n pts =      makeCluster pixel : getPtsByIndex xs pts (n - 1)
    | otherwise =           getPtsByIndex xs pts (n - 1)

{-  | displayClusters function

        This function displays the clusters
-}
displayClusters :: [Cluster] -> IO ()
displayClusters [] = return ()
displayClusters (x:xs) = displayCluster x >> displayClusters xs

{-  | displayCluster function

    This function displays the cluster using this format:7

        --
        (33,18,109) # reference color
        -
        (0,0) (33,18,109) # cluster's pixels
        (1,2) (1,2,3)

-}
displayCluster :: Cluster -> IO ()
displayCluster referenceColor =
    putStrLn "--" >>
    print (color referenceColor) >>
    putStrLn "-" >>
    displayPixels (pixels referenceColor)

{-  | displayPixels function

        This function displays a list of pixels
-}
displayPixels :: [Pixel] -> IO ()
displayPixels [] = return ()
displayPixels ((pos, color):xs) =
    putStr (show pos) >> putStr " " >> print color >>
    displayPixels xs
