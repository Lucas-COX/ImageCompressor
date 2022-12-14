module Compressor where

import CConfig (CConfig (..), exitMessage)
import Types (
    Pixel (..),
    Color (..),
    Cluster (..),
    defaultCluster,
    defaultColor
    )
import Utils (readMaybeFile)
import ImageParser (readImage)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Word (Word8)
import System.Random (genWord8, StdGen)


getClusterColors :: [Cluster] -> [Color]
getClusterColors = map centr


printClusters :: [Cluster] -> IO ()
printClusters = foldr (\ c -> (>>) (putStr $ show c)) (return ())


randomWord8 :: StdGen -> Word8
randomWord8 g = fst $ genWord8 $ snd $ genWord8 g


randomColor :: StdGen -> Color -> Int -> (Color, StdGen)
randomColor g c 0 =
    randomColor (snd $ genWord8 g) (c {r = fromIntegral $ fst $ genWord8 g}) 1
randomColor g c 1 =
    randomColor (snd $ genWord8 g) (c {g = fromIntegral $ fst $ genWord8 g}) 2
randomColor g c 2 =
    randomColor (snd $ genWord8 g) (c {b = fromIntegral $ fst $ genWord8 g}) 3
randomColor g c _ = (c, g)


createClusters :: Int -> StdGen -> [Cluster]
createClusters 0 g = []
createClusters n g = (defaultCluster {centr = cen}):createClusters (n - 1) g'
    where
        (cen, g') = randomColor g defaultColor 0


emptyCluster :: Cluster -> Cluster
emptyCluster (Cluster centr pxs) = Cluster centr []


distance :: Color -> Color -> Double
distance (Color xa ya za) (Color xb yb zb) =
    sqrt (
        (xb - xa) ^ 2 +
        (yb - ya) ^ 2 +
        (zb - za) ^ 2)


findClosestPoint :: [Color] -> Color -> Maybe Color -> Maybe Color
-- findClosestPoint ctr pt = minimum $ fmap (distance pt) ctr
findClosestPoint (p:ps) p2 c =
    if isNothing c || distance p p2 < distance (fromJust c) p2
    then findClosestPoint ps p2 (Just p)
    else findClosestPoint ps p2 c
findClosestPoint [] _ c = c


closest :: [Color] -> Color -> Color
closest ps p2 = fromMaybe (Color 0 0 0) (findClosestPoint ps p2 Nothing)


assignPixel :: [Cluster] -> Pixel -> Color -> [Cluster]
assignPixel (cl:cls) p color = if centr cl == color
    then cl {pxs = pxs cl ++ [p]}:cls
    else cl:assignPixel cls p color
assignPixel [] p cos = []


assignClusters :: [Cluster] -> [Pixel] -> [Cluster]
assignClusters = foldl
    (\ cs p -> assignPixel cs p $ closest (getClusterColors cs) (c p))


distanceList :: [Color] -> [Color] -> [Double]
distanceList (c1:c1s) (c2:c2s) = distance c1 c2:distanceList c1s c2s
distanceList [] _ = []
distanceList _ [] = []


tryDistances :: [Double] -> Double -> Bool
tryDistances (d:ds) lim = d < lim && tryDistances ds lim
tryDistances [] _ = True


mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)


meanPixels :: [Pixel] -> Color
meanPixels ps = Color
    (mean $ map (r . c) ps)
    (mean $ map (g . c) ps)
    (mean $ map (b . c) ps)


moveClusters :: [Cluster] -> [Cluster]
moveClusters = map (\cl -> if length (pxs cl) /= 0
        then cl {centr = meanPixels (pxs cl)}
        else cl
        )


kMeansLoop :: [Pixel] -> [Cluster] -> Double -> [Cluster]
kMeansLoop img cs lim = if tryDistances distances lim
    then nc else kMeansLoop img nc lim
    where
        clusters :: [Cluster]
        clusters = assignClusters (map emptyCluster cs) img
        nc = moveClusters clusters
        distances = distanceList (getClusterColors clusters)
            $ getClusterColors nc


compressor :: StdGen -> CConfig -> IO ()
compressor g c = readMaybeFile (fromJust $ path c) >>= (\file ->
    case readImage file of
        Nothing -> exitMessage "Couldn't read file"
        Just img -> printClusters $
            kMeansLoop img (createClusters (fromJust $ number c) g)
            $ fromJust $ limit c
    )
