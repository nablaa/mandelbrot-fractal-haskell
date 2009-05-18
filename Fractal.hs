module Fractal where

import Data.Word

getIterColor :: Integer -> Integer -> (Word8, Word8, Word8)
getIterColor iter max | iter == 1 = (255, 255, 255)
                      | iter == 0 = (0, 0, 0)
                      | otherwise = (c, c, c)
                      where c = fromInteger $ (iter * 11) `mod` 256

pixelToCoord :: Integer -> Integer -> Integer -> Integer -> (Double, Double)
pixelToCoord w h i j = (-2.0 + (fromInteger i * dx + dx / 2.0), 1 - (fromInteger j * dy + dy / 2.0))
    where dx = 3.0 / fromInteger w
          dy = 2.0 / fromInteger h

getIter :: Integer -> Double -> Double -> Integer
getIter max x y = getIter' max 0 x y x y

getIter' :: Integer -> Integer -> Double -> Double -> Double -> Double -> Integer
getIter' max k x y x0 y0
    | x * x + y * y > 2 * 2 = k
    | k >= max = k
    | otherwise = getIter' max (k + 1) (x * x - y * y + x0) (2 * x * y + y0) x0 y0
