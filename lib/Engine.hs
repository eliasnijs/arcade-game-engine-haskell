module Engine where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Config

-- TODO (elias): set bottom, top, left and right based on width and height in Config file
bottom, top, left, right :: Int
bottom = floor $ -height / 2

top = floor $ (height + 1) / 2

left = floor $ -width / 2

right = floor $ (width + 1) / 2

type Vec2 = (Int, Int)

data Scene =
  Scene
    { move :: Event -> Scene -> Scene
    , update :: Float -> Scene -> Scene
    , player :: Vec2
    , friendlies :: [Vec2]
    , enemies :: [Vec2]
    , score :: Int
    , mode :: Gamemode
    , next :: Int
    , random :: StdGen
    }

data Gamemode
  = Start
      { pairs :: [(Vec2, Scene)]
      }
  | End
  | SnakeMode
      { movVec :: Vec2
      }
  | FroggerMode
      { roads :: [(Vec2, Int)]
      }

onBoard :: Vec2 -> Bool
onBoard (x, y) = x >= left && x <= right && y >= bottom && y <= top

atBottom :: Vec2 -> Bool
atBottom (x, y) = y == bottom

atTop :: Vec2 -> Bool
atTop (x, y) = y == top

collide :: [Vec2] -> [Vec2] -> ([Vec2], [Vec2])
collide c1 c2 = (c1 \\ c2, c2 \\ c1)

decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

initb :: [a] -> [a]
initb [] = []
initb l = init l
