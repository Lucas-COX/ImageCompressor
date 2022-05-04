module Types (
    Color (..),
    Pixel (..),
    Cluster (..),
    defaultColor,
    defaultPixel,
    defaultCluster,
    ) where

import Data.Word (Word8)
import System.Random


data Color = Color {
    r :: Double,
    g :: Double,
    b :: Double
} deriving (Eq)

instance Show Color where
    show (Color r g b) =
        show (round r, round g :: Int, round b :: Int)


defaultColor :: Color
defaultColor = Color 0 0 0

data Pixel = Pixel {
    c :: Color,
    x :: Int,
    y :: Int
}

instance Show Pixel where
    show (Pixel c x y)  = show (x, y) ++ " " ++ show c

defaultPixel :: Pixel
defaultPixel = Pixel {
    c = defaultColor,
    x = 0,
    y = 0
}


data Cluster = Cluster {
    centr :: Color,
    pxs :: [Pixel]
}

instance Show Cluster where
    show (Cluster c ps) = "--\n" ++ show c
        ++ "\n-\n" ++ concatMap (\a -> show a ++ "\n") ps

defaultCluster :: Cluster
defaultCluster = Cluster defaultColor []
