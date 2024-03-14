{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- FileLib
-}

module FileLib(
    verificationFile,
    launchFile
) where

import ConfigLib
import TypeLib
import Control.Exception
import Data.List.Split
import Text.Read
import Data.Char (isSpace)

{-
    | verificationFile function

    Verifies if the file exists and returns its content
-}
verificationFile :: String -> IO (Maybe String)
verificationFile [] = return Nothing
verificationFile path = catch (fmap Just (readFile path)) handler
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

{-  | launchFile function

    Launches the file parser 
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    file' <- verificationFile (_filePath conf)
    case file' of
        Just content -> case verifyImg (lines content) (Just []) of
            Just img -> print img
            Nothing -> myError "Error: invalid file"
        Nothing -> myError "Error: file not found"
