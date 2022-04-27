module Lib
    (
        distance,
        closest,
        parseFile,
        Point,
        expel
    ) where

import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust, fromMaybe)

type Point = (Double, Double, Double)


testPoints :: [Point]
testPoints = [
    (43, 18, 109), -- 39.28484119328644     0
    (43, 17, 109), -- 40.28484119328644     1
    (45, 18, 111), -- 42.11326831803263     2
    (45, 21, 109), -- 42.89039246875043     3
    (48, 21, 112), -- 45.84227971758844     4
    (33, 18, 109), -- 49.28484119328644     5
    (33, 17, 109), -- 49.33471681440733     6
    (35, 18, 111), -- 47.53105244452176     7
    (35, 21, 109), -- 47.82884493860397     8
    (38, 21, 112)  -- 45.84227971758844     9
    ]

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


loopRemoveIndex :: [a] -> Int -> Int -> [a]
loopRemoveIndex (a:as) i n = if i == n
    then loopRemoveIndex as i (n + 1)
    else a:loopRemoveIndex as i (n + 1)
loopRemoveIndex [] i n = []


removeIndex :: [a] -> Int -> [a]
removeIndex a i = loopRemoveIndex a i 0


sumDistances :: Point -> [Point] -> Double
sumDistances p (p2:ps) = distance p p2 + sumDistances p2 ps
sumDistances _ [] = 0


isFurthest :: Point -> [Point] -> [Point] -> Bool
isFurthest p (p2:ps) os =
    (sumDistances p ((p2:ps) ++ os) > sumDistances p2 ((p:ps) ++ os)) && isFurthest p ps (os ++ [p2])
isFurthest _ [] _ = True


findFurthest :: [Point] -> [Point] -> Int -> Int
findFurthest (p:ps) os i =
    if isFurthest p (ps ++ os) []
        then i
        else findFurthest ps (os ++ [p]) (i + 1)
findFurthest _ _ i = i


removeFurthest :: [Point] -> [Point]
removeFurthest ps = removeIndex ps (findFurthest ps [] 0)


loopExpel :: [Point] -> Int -> IO [Point]
loopExpel ps i = if i /= 5
    then print ps >> loopExpel (removeFurthest ps) (i + 1)
    else return ps


expel :: [Point] -> IO [Point]
expel ps = loopExpel testPoints 0
