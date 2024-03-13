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
import System.IO
import ConfigLib
import Control.Exception

verificationFile :: String -> IO (Maybe String)
verificationFile [] = return Nothing
verificationFile path = catch (fmap Just (readFile path)) handler
  where
    handler :: IOException -> IO (Maybe String)
    handler _ = return Nothing

launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    file <- verificationFile (_filePath conf)
    case file of
        Just content -> putStrLn content
        Nothing -> myError "Error: file not found"
