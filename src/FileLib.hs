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

removeLeadingSpaces :: String -> String
removeLeadingSpaces = dropWhile isSpace

verifyColorValue :: Maybe Color -> Maybe Color
verifyColorValue Nothing = Nothing
verifyColorValue (Just (x, y, z))
    | x >= 0 && x <= 255 && y >= 0 && y <= 255 && z >= 0 && z <= 255 =
        Just(x, y, z)
    | otherwise = Nothing

verifyPoint :: String -> Maybe Point
verifyPoint str = readMaybe (removeLeadingSpaces str) :: Maybe Point

verifyColor :: String -> Maybe Color
verifyColor str =
    verifyColorValue (readMaybe (removeLeadingSpaces str) :: Maybe Color)

returnLine :: Maybe Point -> Maybe Color -> Maybe Line
returnLine Nothing _ = Nothing
returnLine _ Nothing = Nothing
returnLine (Just point) (Just color) = Just(point, color)

verifyLine :: String -> Maybe Line
verifyLine str =
    returnLine (verifyPoint (tuple !! 0)) (verifyColor (tuple !! 1))
    where
        tuple = split (keepDelimsR $ oneOf ")") str

-- ALTERNATIVE TO verifyImg FUNCTION, SAME SPEED
-- verifyLine :: String -> Maybe Line
-- verifyLine str = do
--     let splitList = split (keepDelimsR $ oneOf ")") str
--     case splitList of
--         [point, color, _] -> returnLine (verifyPoint point) (verifyColor color)
--         _ -> Nothing

verifyImg :: [String] -> Maybe [Line] -> Maybe In
verifyImg (x:xs) (Just in') = case verifyLine x of
    Just line -> verifyImg xs (Just (line : in'))
    Nothing -> Nothing
verifyImg _ Nothing = Nothing
verifyImg [] (Just in') = Just (In in')

getNbPixels :: In -> Int -> Int
getNbPixels (In []) acc = acc
getNbPixels (In (_:xs)) acc = getNbPixels (In xs) (acc + 1)

{-  | launchFile function

    Gets the file content and launches the file parser
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
    
fileParser :: Maybe String -> IO (In)
fileParser Nothing = myError "Error:\n\tfile not found" >> return (In [])
fileParser (Just "") = myError "Error:\n\tempty file" >> return (In [])
fileParser (Just content) = case verifyImg (lines content) (Just []) of
    Just img -> print img >> return img
    Nothing -> myError "Error:\n\tinvalid file" >> return (In [])

checkNbPixels :: Int -> Int -> IO ()
checkNbPixels nbPixels' nbColors'
    | nbPixels' < nbColors' = myError "Error:\n\tnot enough pixels" >> return ()
    | otherwise = return ()
