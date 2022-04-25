module Lib
    (
        distance,
        closest,
        parseFile,
        Point
    ) where

import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust, fromMaybe)

type Point = (Double, Double, Double)


parseFileLines :: [String] -> [Point] -> Maybe [Point]
parseFileLines (a:as) ps = case (readMaybe a :: Maybe Point) of
    Nothing -> Nothing
    Just any -> parseFileLines as $ ps ++ [any]
parseFileLines [] ps = Just ps


parseFile :: String -> Maybe [Point]
parseFile file = parseFileLines (lines file) []


distance :: Point -> Point -> Double
distance (xa, ya, za) (xb, yb, zb) = sqrt ((xb - xa) ^ 2 + (yb - ya) ^ 2 + (zb - za) ^ 2)


findClosestPoint :: [Point] -> Point -> Maybe Point -> Maybe Point
findClosestPoint (p:ps) p2 c = if isNothing c || distance p p2 < distance p (fromJust c)
    then findClosestPoint ps p2 (Just p)
    else findClosestPoint ps p2 c
findClosestPoint [] _ c = c


closest :: [Point] -> Point -> Point
closest ps p2 = fromMaybe (0, 0, 0) (findClosestPoint ps p2 Nothing)
