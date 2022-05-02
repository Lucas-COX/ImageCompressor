module Main where

import CConfig (CConfig (), getOpts, defaultConf, checkConfig, exitError)
import Compressor (compressor)
import ImageParser (readImage)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random (StdGen (..), newStdGen)

main :: IO ()
main =
    getArgs >>= ( \args ->
        newStdGen >>= (\gen ->
            if checkConfig $ getOpts defaultConf args
            then compressor gen $ getOpts defaultConf args
            else exitError $ getOpts defaultConf args
        )
    )