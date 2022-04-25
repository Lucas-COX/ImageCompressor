module Main where

import System.Environment (getArgs)
import Lib (closest, parseFile, Point ())
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    case parseFile content of
        Nothing -> print "Invalid arguments"
        Just x -> if isNothing (readMaybe (args !! 1) :: Maybe Point)
            then print "Invalid arguments"
            else print (closest x (fromJust (readMaybe (args !! 1) :: Maybe Point)))
