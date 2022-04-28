module ImageParser (
    readImage
) where

import Types (Pixel (..), defaultPixel)
import Utils (getEndOfList, listToMaybe)
import Data.String (words)
import Data.List (tail)
import Data.Word (Word8)
import Text.Read (readMaybe)

parseColor :: String -> Maybe Pixel
parseColor s = case readMaybe s :: Maybe (Word8, Word8, Word8) of --todo error handling for x < 0 && x > 255
    Nothing -> Nothing
    Just (r, g, b) -> Just (defaultPixel {r = r, g = g, b = b})


parsePosition :: String -> Maybe Pixel -> Maybe Pixel
parsePosition _ Nothing = Nothing
parsePosition s (Just p) = case readMaybe s :: Maybe (Int, Int) of
    Nothing -> Nothing
    Just (x, y) -> Just (p {x = x, y = y})


parseLine :: String -> Maybe Pixel
parseLine s =
    parsePosition (head $ words s) $ parseColor =<< getEndOfList (words s)


readImage :: Maybe String -> Maybe [Pixel]
readImage Nothing = Nothing
readImage (Just file) = listToMaybe $ map parseLine (lines file)
