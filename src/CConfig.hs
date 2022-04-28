module CConfig (
    CConfig (..),
    defaultConf,
    getOpts,
    checkConfig,
    exitError,
    exitMessage
    ) where

import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import System.Exit (ExitCode (ExitFailure), exitWith)

data CConfig = CConfig {
    number :: Maybe Int,
    limit :: Maybe Double,
    path :: Maybe String
}

exitMessage :: String -> IO ()
exitMessage msg = putStrLn msg >> exitWith (ExitFailure 84)

exitError :: CConfig -> IO ()
exitError c
    | isNothing (number c) = exitMessage "Invalid number of colors"
    | isNothing (limit c) = exitMessage "Invalid limit"
    | isNothing (path c) = exitMessage "Invalid path"
    | otherwise = exitMessage "Invalid arguments"


defaultConf :: CConfig
defaultConf = CConfig {
    number = Nothing,
    limit = Nothing,
    path = Nothing
}

checkConfig :: CConfig -> Bool
checkConfig c
    | isNothing (number c) || fromJust (number c) <= 0 = False
    | isNothing (limit c) || fromJust (limit c) < 0 = False
    | isNothing (path c) = False
    | otherwise = True


getOpts :: CConfig -> [String] -> CConfig
getOpts c ("-n":value:rest) =
    getOpts (c {number = readMaybe value :: Maybe Int}) rest
getOpts c ("-l":value:rest) =
    getOpts (c {limit = readMaybe value :: Maybe Double}) rest
getOpts c ("-f":value:rest) =
    getOpts (c {path = Just value}) rest
getOpts c _ = c
