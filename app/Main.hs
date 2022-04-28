module Main where

import CConfig (CConfig (), getOpts, defaultConf, checkConfig, exitError)
import Compressor (compressor)
import ImageParser (readImage)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main =
    getArgs >>= ( \args ->
            if checkConfig $ getOpts defaultConf args
            then compressor $ getOpts defaultConf args
            else exitError $ getOpts defaultConf args
        )