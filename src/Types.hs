module Types (
    Pixel (..),
    defaultPixel,
    printPixels
    ) where
import Data.Word (Word8)


data Pixel = Pixel {
    r :: Word8,
    g :: Word8,
    b :: Word8,
    x :: Int,
    y :: Int
}

instance Show Pixel where
    show (Pixel r g b x y)  = show (x, y) ++ " " ++ show (r, g, b)


defaultPixel :: Pixel
defaultPixel = Pixel {
    r = 0,
    g = 0,
    b = 0,
    x = 0,
    y = 0
}

printPixels :: [Pixel] -> IO ()
printPixels = foldr ((>>) . print) (return ())
