module Distance
    ( distance
    ) where

import Text.Read (readMaybe)

type Color = (Int, Int, Int)

distance :: [String] -> IO ()
distance args = do
    let colors = verifyArgs args
    print colors
    case colors of
        Just [Just x, Just y] -> print (colorDistance x y)
        Just [Nothing, _] -> putStrLn "Invalid first color"
        Just [_, Nothing] -> putStrLn "Invalid second color"
        Just _ -> putStrLn "Invalid arguments"
        Nothing -> putStrLn "Invalid arguments"

verifyArgs :: [String] -> Maybe [Maybe Color]
verifyArgs [x, y] = Just (getColors [x, y])
verifyArgs _ = Nothing

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
