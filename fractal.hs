module Fractal where

import Data.Word
import Data.Char
import Control.Monad
import Foreign.Storable
import Foreign.Ptr
import Graphics.UI.SDL as SDL


printFractal :: Integer -> Integer -> Integer -> IO ()
printFractal w h max = putStr $ fractalString w h max

fractalString :: Integer -> Integer -> Integer -> String
fractalString w h max = unlines [fractalStringRow w h max j | j <- [0..h]]

fractalStringRow :: Integer -> Integer -> Integer -> Integer -> String
fractalStringRow w h max j = [fractalChar w h max i j | i <- [0..w]]

fractalChar :: Integer -> Integer -> Integer -> Integer -> Integer -> Char
fractalChar w h max i j = c
    where (x, y) = pixelToCoord w h i j
          iter = getIter max x y
          c = iterToChar (fromIntegral iter)

iterToChar :: Int -> Char
iterToChar n = chr (ord '0' + n `mod` 10)

drawFractal :: Surface -> Integer -> Integer -> Integer -> IO ()
drawFractal s w h max = forM_ [(i, j) | i <- [0..w], j <- [0..h]] (drawPixel s w h max)

drawPixel :: Surface -> Integer -> Integer -> Integer -> (Integer, Integer) -> IO ()
drawPixel s w h max (i, j) = setPixel s i j c
    where c = getPixelColor w h i j max

--Color 255 (fromIntegral (i `mod` 255)) (fromIntegral (j `mod` 255))
--getPixelColor w h i j max
--Color (fromIntegral (i `mod` 256)) (fromIntegral (j `mod` 255)) (fromIntegral (j `mod` 255)) --getPixelColor w h i j max -- Color 255 255 255


toInt :: (Integral a) => a -> Int
toInt n = fromIntegral n

setPixel :: Surface -> Integer -> Integer -> Color -> IO ()
setPixel s i j (Color r g b) = do
  let format = surfaceGetPixelFormat s
  bpp <- pixelFormatGetBytesPerPixel format
  pixels <- surfaceGetPixels s
  let pitch = surfaceGetPitch s
  let offset = toInt j * (toInt pitch) + toInt i * (toInt bpp)
  let pixelPtr = pixels `plusPtr` offset
  pixel <- mapRGB format r g b
  case bpp of
    1 -> error "TODO: bpp 1" -- TODO
    2 -> error "TODO: bpp 2" -- TODO
    3 -> error "TODO: bpp 3" -- TODO
    4 -> poke pixelPtr pixel


getPixelColor :: Integer -> Integer -> Integer -> Integer -> Integer -> Color
getPixelColor w h i j max = colorWrapper $ getIterColor (getIter max x y) max
    where (x, y) = pixelToCoord w h i j

colorWrapper :: (Word8, Word8, Word8) -> Color
colorWrapper (r, g, b) = Color r g b

getIterColor :: Integer -> Integer -> (Word8, Word8, Word8)
getIterColor iter max | iter == 1 = (255, 255, 255)
                      | iter == 0 = (0, 0, 0)
                      | otherwise = (c, c, c)
                      where c = fromInteger $ iter `mod` 256

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


width = 300
height = 200
maxIter = 255


main = do
  SDL.init [InitEverything]
  setVideoMode width height 32 []
  screen <- getVideoSurface
  lockSurface screen
  drawFractal screen (toInteger width) (toInteger height) maxIter
  unlockSurface screen
  updateRect screen (Rect 0 0 width height)
  eventHandler


{-
main = do
  printFractal 100 60 100
-}


eventHandler :: IO ()
eventHandler = do
  e <- waitEvent
  case e of
    Quit -> return ()
    otherwise -> eventHandler