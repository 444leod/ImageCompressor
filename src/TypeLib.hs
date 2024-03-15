{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- TypeLib
-}

module TypeLib(
    Point,
    Short,
    Color,
    Line,
    Cluster,
    In(..),
    Out
) where

type Point = (Int, Int)

newtype Short = Short Int deriving (Show, Read)

type Color = (Int, Int, Int)

type Line = (Point, Color)

newtype Cluster = Cluster (Color, [Line]) deriving (Show)

newtype In = In [Line] deriving (Show)

type Out = [Cluster]
