{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Algorithm
-}

module Algorithm (
    doAlgorithmWithClusters
) where

import Cluster
import Types
import ColorDistance

{-
    | getUpdatedCls function

    This function updates the clusters using the pixel list to regroup the
        pixels by their closest updated cluster
-}
getUpdatedCls :: [Pixel] -> [Cluster] -> [Cluster]
getUpdatedCls pixArray clusters =
    updateClusters pixArray (redoClusters clusters)

{-
    | doAlgorithm function

    This function does the algorithm to update the clusters and return them
    if the clusters reach the maximum number of iterations or reach the
    convergence limit, it displays the clusters
-}
doAlgorithm :: [Cluster] -> [Pixel] -> Float -> Int -> [Cluster]
doAlgorithm clusters _ _ 0 =clusters
doAlgorithm clusters pixArray e n
    | isEnoughDifferent clusters e =
        doAlgorithm (getUpdatedCls pixArray clusters) pixArray e (n - 1)
    | otherwise = clusters

{-
    | isEnoughDifferent function

    This function checks if the clusters are different enough to stop the
        algorithm, based on the convergence limit
-}
isEnoughDifferent :: [Cluster] -> Float -> Bool
isEnoughDifferent [] _ = False
isEnoughDifferent (cl:cls) e
    | colorDistance (color cl) (oldColor cl) > e = True
    | otherwise = isEnoughDifferent cls e

{-
    | doAlgorithmWithClusters function

    This function does the first iteration of the algorithm with the base
        clusters and the pixel list
-}
doAlgorithmWithClusters :: [Cluster] -> [Pixel] -> Float -> [Cluster]
doAlgorithmWithClusters clusters pixList conv =
    doAlgorithm (getUpdatedCls pixList clusters) pixList conv 99
