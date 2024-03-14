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
import System.IO
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
verifyLine str = do

    let tuple = split (keepDelimsR $ oneOf ")") str
    returnLine (verifyPoint (tuple !! 0)) (verifyColor (tuple !! 1))

verifyImg :: [String] -> Maybe In
verifyImg (x:xs) =
    let line = verifyLine x
    in case line of
        Nothing -> Nothing
        Just line -> Just(In[line])

{-  | launchFile function

    Launches the file parser 
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    let line = (split (keepDelimsR $ oneOf ")") "(8  ,0) (289,243,245)")
    print (removeLeadingSpaces (line !! 0))
    print (removeLeadingSpaces (line !! 1))
    print (verifyColor ("(269,243,245)"))
    print (verifyColor ("(229,243,245)"))
    print (verifyPoint (line !! 0))
    print (verifyColor (line !! 1))
    file <- verificationFile (_filePath conf)
    case file of
        Just content -> print(verifyImg (lines content))
        Nothing -> myError "Error: file not found"
