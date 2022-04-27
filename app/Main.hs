module Main where

import System.Environment (getArgs)
import Lib (closest, parseFile, Point (), expel)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    case parseFile content of
        Nothing -> print "Invalid arguments"
        Just x -> expel x >>= print
