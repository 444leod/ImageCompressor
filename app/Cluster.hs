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
    displayClusters,
    updateClusters,
    redoClusters,
) where

import Types
import IsInList
import ColorDistance

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
makeCluster (_, color) = Cluster {color = color, pixels = [], oldColor = color}

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
displayClusters = foldr ((>>) . displayCluster) (return ())

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
displayPixels :: [PixelWithDistances] -> IO ()
displayPixels [] = return ()
displayPixels (((pos, color), _):xs) =
    putStr (show pos) >> putStr " " >> print color >>
    displayPixels xs

-- dispDis :: [Distance] -> IO ()
-- dispDis =
--     foldr (\ x -> (>>) (putStr (printf "%.2f" x) >> putStr " ")) (return ())

{-
    | updateClusterByPixelDist function

    This function updates the clusters by adding the pixels
        its closest cluster based on the pixel's distances
-}
updateClusterByPixelDist :: PixelWithDistances -> [Cluster] -> [Cluster]
updateClusterByPixelDist (pixel, dis) clusters =
    addPixelToCluster (pixel, dis) clusters (getMinimumIndex dis)

{-
    | addPixelToCluster function

    This function adds a pixel to a cluster by the cluster's index
-}
addPixelToCluster :: PixelWithDistances -> [Cluster] -> Int -> [Cluster]
addPixelToCluster pixel (cl:cls) 0 =
    cl {pixels = pixel : pixels cl} : cls
addPixelToCluster pixel (cl:cls) index =
    cl : addPixelToCluster pixel cls (index - 1)
addPixelToCluster _ clusters _ = clusters

{-
    | updateClusterByPixel function

    This function updates the clusters by adding the pixels
        its closest cluster, by calculating the distances between the pixel
        and the clusters
-}
updateClusterByPixel :: [Cluster] -> Pixel -> [Cluster]
updateClusterByPixel cls pixel =
    updateClusterByPixelDist (getPixelWithDistance pixel cls) cls

{-
    | updateClusters function

    This function updates the clusters contents, by adding each pixel
        to its closest cluster
-}
updateClusters :: [Pixel] -> [Cluster] -> [Cluster]
updateClusters (x:xs) clusters =
    updateClusters xs (updateClusterByPixel clusters x)
updateClusters _ clusters = clusters

{-
    | getPixelWithDistance function

    This function gets the pixel with its distances to all the clusters
-}
getPixelWithDistance :: Pixel -> [Cluster] -> PixelWithDistances
getPixelWithDistance pix clusters = (pix, getDistances (snd pix) clusters)

{-
    | getDistances function

    This function gets the distances between a pixel and all the clusters
-}
getDistances :: Color -> [Cluster] -> [Distance]
getDistances baseColor (cl:cls) =
    colorDistance baseColor (color cl) : getDistances baseColor cls
getDistances _ _ = []

{-
    | getMinimumIndex function

    This function gets the index of the minimum distance in a list
-}
getMinimumIndex :: [Distance] -> Int
getMinimumIndex (x:xs) = getMinimumIndex' xs x 0 1
getMinimumIndex _ = 0

{-
    | getMinimumIndex' function

    This function gets the index of the minimum distance in a list
-}
getMinimumIndex' :: [Distance] -> Distance -> Int -> Int -> Int
getMinimumIndex' (x:xs) min minIndex currentIndex
    | x < min = getMinimumIndex' xs x currentIndex (currentIndex + 1)
    | otherwise = getMinimumIndex' xs min minIndex (currentIndex + 1)
getMinimumIndex' _ _ minIndex _ = minIndex

{-
    | redoClusters function

    This function resets the clusters by setting thr current color to the old
        color, getting the new average color and
        setting the pixels to an empty list
-}
redoClusters :: [Cluster] -> [Cluster]
redoClusters (cl:cls) = cl {
    color = getNewColor (pixels cl),
    pixels = [],
    oldColor = color cl
    } : redoClusters cls
redoClusters _ = []


{-  | getNewColor function

    This function gets the new main color of the cluster
    It uses the average of the colors of the pixels in the cluster

    Returns a color
-}
getNewColor :: [PixelWithDistances] -> Color
getNewColor pixels =
    (getAvg (getR pixels), getAvg (getG pixels), getAvg (getB pixels))

{-
    | getR function

    This function gets the red values of the pixels in the cluster

    Returns a list of red values
-}
getR :: [PixelWithDistances] -> [Int]
getR (((_, (r, _, _)), _):xs) = r : getR xs
getR _ = []

{-
    | getG function

    This function gets the green values of the pixels in the cluster

    Returns a list of green values
-}
getG :: [PixelWithDistances] -> [Int]
getG (((_, (_, g, _)), _):xs) = g : getG xs
getG _ = []

{-
    | getB function

    This function gets the blue values of the pixels in the cluster

    Returns a list of blue values
-}
getB :: [PixelWithDistances] -> [Int]
getB (((_, (_, _, b)), _):xs) = b : getB xs
getB _ = []

{-  | getAvg function

    This function gets the average of a list of integers

    Returns an integer
-}
getAvg :: [Int] -> Int
getAvg [] = 0
getAvg x = sum x  `div` length x
