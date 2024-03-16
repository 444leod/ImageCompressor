{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Types
-}

module Types (
    Cluster(..),
    Pixel,
    Position,
    Color,
    Distance,
    PixelWithDistances,
    In(..),
    Short(..),
    Line
) where

{- | Types module

    This module contains the data types used in the project

    Types:
        - Pixel
        - Position
        - Color
        - Conf

    Pixel: A pixel is a tuple of a position and a color
        (retrieved from the image)
    Position: A position is a tuple of two integers
        (x and y coordinates)
    Color: A color is a tuple of three integers
        (red, green and blue)
    Conf: A configuration is a record containing the number of clusters
        and the number of lines in the image
-}
type Pixel = (Position, Color)
type Position = (Int, Int)
type Color = (Int, Int, Int)

{-  | Cluster data type

    This data type represents a cluster, composed of a
        representative color and a list of pixels, which are
        the pixels that are closest to the representative color
-}
data Cluster = Cluster {color :: Color, pixels :: [PixelWithDistances], oldColor :: Color}

newtype In = In [Pixel] deriving (Show)

newtype Short = Short Int deriving (Show, Read)

type Line = (Position, Color)

type PixelWithDistances = (Pixel, [Distance])
type Distance = Float
