{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- FileLib
-}

module FileLib(
    launchFile
) where

import ConfigLib
import TypeLib
import Control.Exception
import Data.List.Split
import Text.Read
import Data.Char (isSpace)

{-
    | getFileContents function

    Verifies if the file exists and returns its content
-}
getFileContents :: String -> IO (Maybe String)
getFileContents [] = return Nothing
getFileContents path = catch (fmap Just (readFile path)) handler
  where
    handler :: IOException -> IO (Maybe String)
    handler _ = return Nothing

{-
    | removeLeadingSpaces function

    Removes leading spaces from a string
-}
removeLeadingSpaces :: String -> String
removeLeadingSpaces = dropWhile isSpace

{-  | getNbPixels function

    Gets the number of pixels in the parsed file
-}
getNbPixels :: In -> Int -> Int
getNbPixels (In []) acc = acc
getNbPixels (In (_:xs)) acc = getNbPixels (In xs) (acc + 1)

{-  | checkNbPixels function

    Checks if the number of pixels is greater than the number of colors
    Error message if not enough pixels
-}
checkNbPixels :: Int -> Int -> IO ()
checkNbPixels pixels colors'
    | pixels < colors' = myError "Error:\n\tnot enough pixels" >> return ()
    | otherwise = return ()

{-
    | verifyColorValue function

    Verifies if the color value is valid (between 0 and 255)
-}
verifyColorValue :: Maybe Color -> Maybe Color
verifyColorValue Nothing = Nothing
verifyColorValue (Just (x, y, z))
    | x >= 0 && x <= 255 && y >= 0 && y <= 255 && z >= 0 && z <= 255 =
        Just(x, y, z)
    | otherwise = Nothing

{-
    | returnLine function

    Returns a line if the point and the color are valid
-}
returnLine :: Maybe Point -> Maybe Color -> Maybe Line
returnLine Nothing _ = Nothing
returnLine _ Nothing = Nothing
returnLine (Just point) (Just color) = Just(point, color)

{-
    | verifyPoint function

    Parses a point and verifies if it is a valid
-}
verifyPoint :: String -> Maybe Point
verifyPoint str = readMaybe (removeLeadingSpaces str) :: Maybe Point

{-
    | verifyColor function

    Parses a color and verifies if it is a valid
-}
verifyColor :: String -> Maybe Color
verifyColor str =
    verifyColorValue (readMaybe (removeLeadingSpaces str) :: Maybe Color)


{-  | verifyLine function

    Parses a line and verifies if it is a valid
-}
verifyLine :: String -> Maybe Line
verifyLine str =
    returnLine (verifyPoint (tuple !! 0)) (verifyColor (tuple !! 1))
    where
        tuple = split (keepDelimsR $ oneOf ")") str


{-
    | verifyImg function

    Parses the file content and verifies if it is a valid
-}
verifyImg :: [String] -> Maybe [Line] -> Maybe In
verifyImg (x:xs) (Just in') = case verifyLine x of
    Just line -> verifyImg xs (Just (line : in'))
    Nothing -> Nothing
verifyImg _ Nothing = Nothing
verifyImg [] (Just in') = Just (In in')


{-  | fileParser function
    
    Parses the file content
    Error message if the file is invalid, empty or not found
-}
fileParser :: Maybe String -> IO (In)
fileParser Nothing = myError "Error:\n\tfile not found" >> return (In [])
fileParser (Just "") = myError "Error:\n\tempty file" >> return (In [])
fileParser (Just content) = case verifyImg (lines content) (Just []) of
    Just img -> return img
    Nothing -> myError "Error:\n\tinvalid file" >> return (In [])

{-  | launchFile function

    Gets the file content and launches the file parser
    Also saves the number of pixels and the parsed file into conf
-}
launchFile :: VerifiedConf -> IO ()
launchFile oldconf = do
    file' <- getFileContents (_filePath oldconf)
    parsedFile <- fileParser file'
    let conf = oldconf{
        _file = parsedFile,
        _nbPixels = getNbPixels parsedFile 0
    }
    checkNbPixels (_nbPixels conf) (_nbColors conf)
    return ()

-- ALTERNATIVE TO verifyImg FUNCTION, SAME SPEED
-- verifyLine :: String -> Maybe Line
-- verifyLine str = do
--     let splitList = split (keepDelimsR $ oneOf ")") str
--     case splitList of
--         [point, color, _] -> returnLine (verifyPoint point) (verifyColor color)
--         _ -> Nothing
