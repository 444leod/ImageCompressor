{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- ConfigLib
-}

module ConfigLib(
    Conf(..),
    defaultConf,
    getOpts,
    validateConf
) where

import Data.Maybe()
import Data.Char(isDigit)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, hPutStr, stderr)
import Text.Read(readMaybe)

{-  | Conf data

    Store the configuration of the program
-}
data Conf = Conf {
    _nbColors :: Maybe Int,
    _convergenceLimit :: Maybe Float,
    _filePath :: Maybe String
} deriving (Show)

-- Private functions

{-  | intVerification function 

    Check if a string is a number

    Return True if it is, False otherwise
-}
intVerification :: [Char] -> Bool
intVerification = all isDigit


{-  | readInt function

    Read an integer from a string

    Return Just the integer if it is, Nothing otherwise
-}
readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':xs)
    | intVerification xs = Just (-read xs)
    | otherwise = Nothing
readInt xs
    | intVerification xs = Just (read xs)
    | otherwise = Nothing

{-
    | readFloat function

    Read a float from a string

    Return Just the float if it is, Nothing otherwise
-}
readFloat :: [Char] -> Maybe Float
readFloat = readMaybe

{-  | myError function
    
    Print an error message and exit the program with a failure code
-}
myError :: String -> IO ()
myError str =
    hPutStrLn stderr str >>
    hPutStr stderr "Usage:\n\t./imageCompressor -n [uint] -l [uint]" >>
    hPutStrLn stderr " -f [str]" >>
    exitWith (ExitFailure 84)

-- Public functions

{-  | defaultConf value

    Default configuration of the program
-}
defaultConf :: Conf
defaultConf = Conf {
    _nbColors = Nothing,
    _convergenceLimit = Nothing,
    _filePath = Nothing
}

{-  | getOpts function

    Get the options from the command line

    Return Just the configuration if it is valid, Nothing otherwise
-}
getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("-n": x:xs) = case readInt x of
    Nothing -> Nothing
    Just x' -> getOpts conf{_nbColors = Just x'} xs
getOpts conf ("-l": x:xs) = case readFloat x of
    Nothing -> Nothing
    Just x' -> getOpts conf{_convergenceLimit = Just x'} xs
getOpts conf ("-f": x:xs) = getOpts conf{_filePath = Just x} xs
getOpts _ _ = Nothing

{-  | validateConf function

    Validate the configuration

    Return Nothing if it is valid, Just the error message otherwise
-}
validateConf :: Maybe Conf -> IO ()
validateConf Nothing = myError "Error:\n\tMissing arguments."
validateConf (Just (Conf Nothing _ _)) =
    myError "Error:\n\tn is missing."
validateConf (Just (Conf _ Nothing _)) =
    myError "Error:\n\tl is missing."
validateConf (Just (Conf _ _ Nothing)) =
    myError "Error:\n\tf is missing."
validateConf (Just (Conf (Just _nbColors') _ _))
    | _nbColors' < 1 = myError "Error:\n\tn must be greater than 0."
validateConf (Just (Conf _ (Just _convergenceLimit') _))
    | _convergenceLimit' < 0 = myError "Error:\n\tl must be greater than 0."
validateConf _ = return ()
