{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding (null)
import Control.Applicative
import Data.List (find)
import qualified Data.Set as Set 
import Data.Maybe
import Data.Word
import Debug.Trace
import System.Random
import Control.Monad
import qualified Graphics.UI.SDL as SDL

width = 800 
height = 600 
playerSpeed = 5

data RGB = RGB (Word8, Word8, Word8) deriving (Show)
type Point = (Double,Double)
--RGB, speed, x, y 
data Star = Star RGB Double Point 

sumTuples (a1,b1) (a2,b2) = (a1 + a2,b1 + b2)

makeStar :: n -> IO (RGB,Double, Point)
makeStar n = do
    r <- getStdRandom $ randomR (25,255)
    g <- getStdRandom $ randomR (25,255)
    b <- getStdRandom $ randomR (25,255)
    bw <- getStdRandom $ randomR (1,100) :: IO Int
    x <- getStdRandom $ randomR (0,fromIntegral width)
    y <- getStdRandom $ randomR (0,fromIntegral height)
    let starSpeedMod = 50
    let speed = if bw <= 2 
            then (fromIntegral (r + g + b) / 3) / (starSpeedMod / 4)
            else fromIntegral r / starSpeedMod
    if bw <= 1 
        then return (RGB (r, g, b),speed,(x,y))
        else return (RGB (r, r, r),speed,(x,y))

moveStar x' (Star c s (x, y)) = Star c s (x + x', y + s) 

starSpeed (Star _ s _) = s

wrap :: (Ord a) => a -> a -> a -> a
wrap x min max
    | x < min = max
    | x > max = min
    | otherwise = x

wrapStar (Star b s (x, y)) = Star b s (newx, newy)
    where newy = wrap y 0 (fromIntegral height) 
          newx = wrap x 0 (fromIntegral width)

data Hole = Hole

makePixel :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO SDL.Pixel
makePixel = SDL.mapRGB . SDL.surfaceGetPixelFormat 

drawPixel :: SDL.Surface -> SDL.Pixel -> Double -> Double -> IO Bool
drawPixel screen p x y =
    SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 1 1) p

showStar screen (Star (RGB (r, g, b)) _ (sx, sy)) = do
    pixel <- makePixel screen r g b
    drawPixel screen pixel sx sy

playerMovement = [(SDL.SDLK_UP,(0,-playerSpeed)),
                  (SDL.SDLK_DOWN,(0,playerSpeed)), 
                  (SDL.SDLK_LEFT,(-playerSpeed,0)),
                  (SDL.SDLK_RIGHT,(playerSpeed,0))] 

inputToVector :: Set.Set SDL.Keysym -> (Double,Double)
inputToVector keysDown = foldr sumTuples (0,0) mods 
    where 
        keys = SDL.symKey <$> Set.toList keysDown 
        mods = [snd m | m <- playerMovement, fst m `elem` keys]

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode width height 32 [SDL.HWSurface]
  s <- mapM makeStar [0..1000]
  let stars = fmap (\(b, s, p) -> Star b s p) s 
  go Set.empty screen stars (0,0)

 where

  go keysDown screen stars (x,y) = do
    keysDown' <- parseEvents keysDown
    let (x',y') = inputToVector keysDown
    let stars' = fmap (wrapStar . moveStar (-x')) stars 

    makePixel screen 0 0 0 >>=
        SDL.fillRect screen Nothing

    makePixel screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect 400 570 50 50)

    mapM_ (showStar screen) stars 

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go keysDown' screen stars' (x + x',y + y')


justs :: [Maybe a] -> [a]
justs [] = []
justs (Just a:xs) = a : justs xs
justs (Nothing:xs) = justs xs

parseEvents :: Set.Set SDL.Keysym -> IO (Set.Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
    SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set.Set SDL.Keysym -> Bool
keyDown k = not . Set.null . Set.filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym
