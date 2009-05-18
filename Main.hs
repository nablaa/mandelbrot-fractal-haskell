module Main where

import Data.Char
import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.SDL as SDL
import Fractal

---
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
---

drawFractal :: Surface -> Integer -> Integer -> Integer -> IO ()
drawFractal s w h max = forM_ [(i, j) | j <- [0..(h - 1)], i <- [0..(w - 1)]] (drawPixel s w h max)

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


width = 600
height = 400
maxIter = 10


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
  printFractal width height maxIter
-}


eventHandler :: IO ()
eventHandler = do
  e <- waitEvent
  case e of
    Quit -> return ()
    otherwise -> eventHandler