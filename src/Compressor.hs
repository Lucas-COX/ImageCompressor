{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Compressor where

import CConfig (CConfig (..), exitMessage)
import Types (printPixels)
import Utils (readMaybeFile)
import ImageParser (readImage)
import Data.Maybe (fromJust)

compressor :: CConfig -> IO ()
compressor c = readMaybeFile (fromJust $ path c) >>= (\file ->
    case readImage file of
        Nothing -> exitMessage "Couldn't read file"
        Just img -> printPixels img
    )
