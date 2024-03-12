module LineParser
    ( parseLines
    ) where

import Data.Maybe (isNothing)
import Text.Read (readMaybe)

type Pixel = (Maybe Position, Maybe Color)
type Position = (Int, Int)
type Color = (Short, Short, Short)
type Short = Int

getPixel :: String -> Maybe Pixel
getPixel line = parsePixel (words line)

parsePixel :: [String] -> Maybe Pixel
parsePixel [pos, color] = Just (readMaybe pos, readMaybe color)
parsePixel _ = Nothing

parseLines :: [String] -> [Maybe Pixel]
parseLines = map getPixel

isLinesError :: [Maybe Pixel] -> Bool
isLinesError pixels = isNothing (last pixels)
