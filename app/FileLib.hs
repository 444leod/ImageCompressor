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
import Control.Exception
import Text.Read
import Data.Char (isSpace)
import Types
import RandomList
import Cluster
import Algorithm

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
checkNbPixels pixelArr colors'
    | pixelArr < colors' = myError "Error:\n\tnot enough pixels"
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
returnLine :: Maybe Position -> Maybe Color -> Maybe Line
returnLine Nothing _ = Nothing
returnLine _ Nothing = Nothing
returnLine (Just point) (Just colr) = Just(point, colr)

{-
    | verifyPoint function

    Parses a point and verifies if it is a valid
-}
verifyPoint :: String -> Maybe Position
verifyPoint str = readMaybe (removeLeadingSpaces str) :: Maybe Position

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
        tuple = splitOnDelim ')' str

{-
    | splitOnDelim function

    Splits a string on a delimiter
-}
splitOnDelim :: Eq a => a -> [a] -> [[a]]
splitOnDelim _ [] = []
splitOnDelim delim str =
    let (before, remainder) = break (== delim) str
    in (before ++ [delim]) : case remainder of
        [] -> []
        x -> splitOnDelim delim (tail x)

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

{-  | getConfigFromFile function

    Gets the file content and launches the file parser
    Also saves the number of pixels and the parsed file into conf
-}
getConfigFromFile :: VerifiedConf -> IO VerifiedConf
getConfigFromFile oldconf = do
    file' <- getFileContents (_filePath oldconf)
    parsedFile <- fileParser file'
    return (oldconf{
        _file = parsedFile,
        _nbPixels = getNbPixels parsedFile 0
    })

{-  | getInitialClusters function

    generate the base clusters based on the parsed pixels
-}
getInitialClusters :: VerifiedConf -> IO [Cluster]
getInitialClusters conf = do
    checkNbPixels (_nbPixels conf) (_nbColors conf)
    indexes <- getRandomList (_nbColors conf) (1, _nbPixels conf)
    return (
        getClusters (getPixelsFromIn (_file conf)) (_nbPixels conf) indexes)


{-  | launchFile function

    Gets the file content and launches the file parser
    Also saves the number of pixels and the parsed file into conf and launches
        the algorithm
-}
launchFile :: VerifiedConf -> IO ()
launchFile oldconf = do
    conf <- getConfigFromFile oldconf
    clusters <- getInitialClusters conf
    displayClusters (doAlgorithmWithClusters
        (updateClusters (getPixelsFromIn (_file conf)) clusters)
        (getPixelsFromIn (_file conf))
        (_convergenceLimit conf))


{-  | getPixelsFromIn function

    Gets the pixels from the In type (parsed lines from the file)
-}
getPixelsFromIn :: In -> [Pixel]
getPixelsFromIn (In pix) = map (\(pos, col) -> (pos, col)) pix

-- ALTERNATIVE TO verifyImg FUNCTION, SAME SPEED
-- verifyLine :: String -> Maybe Line
-- verifyLine str = do
--     let splitList = split (keepDelimsR $ oneOf ")") str
--     case splitList of
--         [point, color, _] -> returnLine (verifyPoint point) (verifyColor color)
--         _ -> Nothing

