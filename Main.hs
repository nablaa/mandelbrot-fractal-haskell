module Main where

import Data.Char (ord, chr)
import Data.Word (Word8)
import Control.Monad (forM_)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
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
    where (x, y) = pixelToCoord w h (-2.0, 1.0, 1.0, -1.0) i j
          iter = getIter max x y
          c = iterToChar (fromIntegral iter)

iterToChar :: Int -> Char
iterToChar n = chr (ord '0' + n `mod` 10)
---

drawFractal :: Surface -> Integer -> Integer -> Area -> Integer -> IO ()
drawFractal s w h area max = forM_ (getPixelColors w h area max) setPixel'
    where setPixel' (i, j, c) = setPixel s i j c

drawPixel :: Surface -> Integer -> Integer -> Area -> Integer -> (Integer, Integer) -> IO ()
drawPixel s w h area max (i, j) = setPixel s i j c
    where c = getPixelColor w h area i j max

getPixelColors :: Integer -> Integer -> Area -> Integer -> [(Integer, Integer, Color)]
getPixelColors w h area max = [(i, j, getPixelColor w h area i j max) | j <- [0..(h - 1)], i <- [0..(w - 1)]]

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

getPixelColor :: Integer -> Integer -> Area -> Integer -> Integer -> Integer -> Color
getPixelColor w h area i j max = colorWrapper $ getIterColor (getIter max x y) max
    where (x, y) = pixelToCoord w h area i j

colorWrapper :: (Word8, Word8, Word8) -> Color
colorWrapper (r, g, b) = Color r g b

width = 600
height = 400
maxIter = 100
startArea = (-2, 1, 1, -1)

main :: IO ()
main = do
  SDL.init [InitEverything]
  setVideoMode width height 32 []
  screen <- getVideoSurface
  drawScreen screen startArea
  eventHandler screen startArea


drawScreen :: Surface -> Area -> IO ()
drawScreen screen area = do
  lockSurface screen
  drawFractal screen (toInteger width) (toInteger height) area maxIter
  unlockSurface screen
  updateRect screen (Rect 0 0 width height)


{-
main = do
  printFractal width height maxIter
-}


eventHandler :: Surface -> Area -> IO ()
eventHandler screen area = do
  e <- waitEvent
  case e of
    Quit -> return ()
    KeyDown keysym -> keyHandler screen area keysym
    otherwise -> eventHandler screen area

keyHandler :: Surface -> Area -> Keysym -> IO ()
keyHandler screen area@(x0, y0, x1, y1) keysym
    | symKey keysym == SDLK_ESCAPE = return ()
    | symKey keysym == SDLK_RETURN = do 
  drawScreen screen area
  eventHandler screen area
    | symKey keysym == SDLK_UP = do
  drawScreen screen areaUp
  eventHandler screen areaUp
    | symKey keysym == SDLK_DOWN = do
  drawScreen screen areaDown
  eventHandler screen areaDown
    | symKey keysym == SDLK_LEFT = do
  drawScreen screen areaLeft
  eventHandler screen areaLeft
    | symKey keysym == SDLK_RIGHT = do
  drawScreen screen areaRight
  eventHandler screen areaRight
    | symKey keysym == SDLK_PLUS = do
  drawScreen screen areaIn
  eventHandler screen areaIn
    | symKey keysym == SDLK_MINUS = do
  drawScreen screen areaOut
  eventHandler screen areaOut
    | otherwise = eventHandler screen area
    where dx = abs (x1 - x0) / 10
          dy = abs (y1 - y0) / 10
          areaUp = (x0, y0 + dy, x1, y1 + dy)
          areaDown = (x0, y0 - dy, x1, y1 - dy)
          areaLeft = (x0 - dx, y0, x1 - dx, y1)
          areaRight = (x0 + dx, y0, x1 + dx, y1)
          areaIn = (x0 + dx, y0 - dy, x1 - dx, y1 + dy)
          areaOut = (x0 - dx, y0 + dy, x1 + dx, y1 - dy)

