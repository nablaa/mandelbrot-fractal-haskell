module Fractal where

import Data.Word (Word8)

type Area = (Double, Double, Double, Double) -- x0, y0, x1, y1

getIterColor :: Integer -> Integer -> (Word8, Word8, Word8)
getIterColor iter max | iter == 1 = (255, 255, 255)
                      | iter == 0 = (0, 0, 0)
                      | otherwise = (c, c, c)
                      where c = fromInteger $ (iter * 11) `mod` 256

pixelToCoord :: Integer -> Integer -> Area -> Integer -> Integer -> (Double, Double)
pixelToCoord w h (x0, y0, x1, y1) i j = (x0 + (fromInteger i * dx + dx / 2.0), y0 - (fromInteger j * dy + dy / 2.0))
    where dx = abs (x1 - x0) / fromInteger w
          dy = abs (y1 - y0) / fromInteger h

getIter :: Integer -> Double -> Double -> Integer
getIter max x y = getIter' max 0 x y x y

getIter' :: Integer -> Integer -> Double -> Double -> Double -> Double -> Integer
getIter' max k x y x0 y0
    | x * x + y * y > 2 * 2 = k
    | k >= max = k
    | otherwise = getIter' max (k + 1) (x * x - y * y + x0) (2 * x * y + y0) x0 y0
