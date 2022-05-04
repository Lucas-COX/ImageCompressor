module ImageParser (
    readImage
) where

import Types (Pixel (..), Color (..), defaultPixel)
import Utils (getEndOfList, listToMaybe)
import Data.String (words)
import Data.List (tail)
import Text.Read (readMaybe)


validColor :: (Double, Double, Double) -> Bool
validColor (r, g, b)
    | r < 0 || r > 255 = False
    | g < 0 || g > 255 = False
    | b < 0 || g > 255 = False
    | otherwise = True


parseColor :: String -> Maybe Pixel
parseColor s = case readMaybe s :: Maybe (Double, Double, Double) of
    Nothing -> Nothing
    Just (r, g, b) -> if validColor (r, g, b)
        then Just (defaultPixel {c = Color r g b})
        else Nothing


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
