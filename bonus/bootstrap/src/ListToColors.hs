module ListToColors
    ( listToColors
    ) where

import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)

type Color = (Int, Int, Int)
type ColorDistance = (Color, Color, Double)

listToColors :: [String] -> IO ()
listToColors [file, compareColor] = do
    colors <- getColorsFromFile file
    let areColors = Nothing `notElem` colors && isJust (stringToColor compareColor)
    if areColors
        then listToColors' (map fromJust colors) (fromJust (stringToColor compareColor))
        else putStrLn "Invalid colors"
listToColors _ = putStrLn "Invalid arguments"

listToColors' :: [Color] -> Color -> IO ()
listToColors' colors compareColor = do
    let colorDistanceList = distance colors compareColor
    printList colorDistanceList
    print (getClosestColorDistance colorDistanceList)
    return ()

getClosestColorDistance :: [ColorDistance] -> Color
getClosestColorDistance list = getColorFromColorDistance (getClosestColorDistance' list)

getColorFromColorDistance :: ColorDistance -> Color
getColorFromColorDistance (x, _, _) = x

getClosestColorDistance' :: [ColorDistance] -> ColorDistance
getClosestColorDistance' [x] = x
getClosestColorDistance' ((color1, color2, distance1):(color3, color4, distance2):xs)
    | distance1 < distance2 = getClosestColorDistance' ((color1, color2, distance1):xs)
    | otherwise = getClosestColorDistance' ((color3, color4, distance2):xs)
getClosestColorDistance' [] = error "Empty list"

printList :: [ColorDistance] -> IO ()
printList [] = return ()
printList ((x, y, z):xs) = do
    putStrLn $ "Color " ++ show x ++ " is " ++ show z ++ " away from " ++ show y
    printList xs

getColorsFromFile :: String -> IO [Maybe Color]
getColorsFromFile file = do
    contents <- readFile file
    return (getColors (lines contents))

getColors :: [String] -> [Maybe Color]
getColors = map stringToColor

stringToColor :: String -> Maybe Color
stringToColor = readMaybe

colorDistance :: Color -> Color -> Double
colorDistance (r1, g1, b1) (r2, g2, b2) = sqrt (dr * dr + dg * dg + db * db)
  where
    dr = fromIntegral (r1 - r2)
    dg = fromIntegral (g1 - g2)
    db = fromIntegral (b1 - b2)


distance :: [Color] -> Color -> [ColorDistance]
distance [] _ = []
distance (x:xs) y = (x, y, colorDistance x y) : distance xs y

