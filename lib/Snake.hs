module Snake
  ( snakescene
  ) where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Config
import End
import Engine

snakescene :: StdGen -> Scene
snakescene =
  Scene snakemove snakeupdate (0, 0) [(0, 1)] [] 1 (SnakeMode (1, 0)) (-1)

snakeupdate :: Float -> Scene -> Scene
snakeupdate _ (Scene m u (px, py) f e s (SnakeMode (mx, my)) n t)
  | any (\(ex, ey) -> ex == px && ey == py) e =
    Scene m u (px, py) f e s (SnakeMode (mx, my)) 1 t
  | otherwise =
    let tf = snd $ collide [(px, py)] f
        ta
          | length tf == length f = []
          | otherwise = [last e]
        (rx, t') = randomR (left, right) t
        (ry, t'') = randomR (bottom, top) t'
        food
          | length tf == length f = f
          | otherwise = fst $ collide [(rx, ry)] e
        score
          | length tf == length f = s
          | otherwise = s + 1
     in Scene
          m
          u
          (npx, npy)
          food
          ((px, py) : initb e ++ ta)
          score
          (SnakeMode (mx, my))
          n
          t''
  where
    npx = (px + mx + right) `mod` (round width + 1) + left
    npy = (py + my + top) `mod` (round height + 1) + bottom
-- TODO (elias): set nextlevel 
snakeupdate _ s = s

snakemove :: Event -> Scene -> Scene
snakemove (EventKey (SpecialKey KeyLeft) Down _ _) scene =
  scene {mode = SnakeMode (-1, 0)}
snakemove (EventKey (SpecialKey KeyRight) Down _ _) scene =
  scene {mode = SnakeMode (1, 0)}
snakemove (EventKey (SpecialKey KeyDown) Down _ _) scene =
  scene {mode = SnakeMode (0, -1)}
snakemove (EventKey (SpecialKey KeyUp) Down _ _) scene =
  scene {mode = SnakeMode (0, 1)}
snakemove _ g = g
