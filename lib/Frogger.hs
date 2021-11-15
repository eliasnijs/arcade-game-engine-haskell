module Frogger
  ( froggerscene
  ) where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Config
import End
import Engine

constructroads :: [(Vec2, Int)]
-- TODO (elias): generate random x from {left, 0, right}
constructroads = (\(x, y) -> ((x, y), -x `div` abs x)) <$> t
  where
    t = [(left, y) | y <- [bottom .. top], y `mod` 3 /= 0]

froggerscene :: StdGen -> Scene
froggerscene =
  Scene
    froggermove
    froggerupdate
    (0, bottom)
    []
    []
    0
    (FroggerMode constructroads)
    (-1)

endcheck :: Scene -> Scene
endcheck scene
  | atTop (player scene) = scene {next = 1}
  | any (\(ex, ey) -> ex == fst p && ey == snd p) (enemies scene) =
    scene {next = 1}
  | otherwise = scene
  where
    p = player scene

-- TODO (elias): small chanch of spawning car
spawncars :: [(Vec2, Int)] -> [Vec2] -> [Vec2]
spawncars roads enemies =
  concat [[(x - dir, y), (x, y)] | ((x, y), dir) <- roads]

movecars :: [(Vec2, Int)] -> [Vec2] -> [Vec2]
movecars roads enemies =
  [ ((+) x $ snd $ head roads, y)
  | (x, y) <- enemies
  , onBoard ((+) x $ snd $ head roads, y)
  ]

froggerupdate :: Float -> Scene -> Scene
froggerupdate _ scene = endcheck s
  where
    s =
      scene {score = score scene + 1, enemies = movecars r (e ++ spawncars r e)}
      where
        r = roads (mode scene)
        e = enemies scene

froggermove :: Event -> Scene -> Scene
froggermove (EventKey (SpecialKey KeyUp) Down _ _) scene = endcheck s
  where
    s = scene {player = (fst $ player scene, incBound (snd $ player scene) top)}
froggermove (EventKey (SpecialKey KeyDown) Down _ _) scene = endcheck s
  where
    s =
      scene
        {player = (fst $ player scene, decBound (snd $ player scene) bottom)}
froggermove (EventKey (SpecialKey KeyRight) Down _ _) scene = endcheck s
  where
    s =
      scene {player = (incBound (fst $ player scene) right, snd $ player scene)}
froggermove (EventKey (SpecialKey KeyLeft) Down _ _) scene = endcheck s
  where
    s =
      scene {player = (decBound (fst $ player scene) left, snd $ player scene)}
froggermove _ g = g
