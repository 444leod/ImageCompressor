{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- ColorDistance
-}

module ColorDistance (colorDistance) where

import Types

{-
    | colorDistance function

    This function calculates the distance between two colors

    Returns a float
-}
colorDistance :: Color -> Color -> Distance
colorDistance (r1, g1, b1) (r2, g2, b2) = sqrt (dr * dr + dg * dg + db * db)
  where
    dr = fromIntegral (r1 - r2)
    dg = fromIntegral (g1 - g2)
    db = fromIntegral (b1 - b2)
