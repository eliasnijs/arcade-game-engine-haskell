module Manager where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Config
import Engine
import Renderer

import End
import Start

import Frogger
import Snake

selectionsmenu :: StdGen -> [(Vec2, Scene)]
selectionsmenu r = [((0, 3), snakescene r), ((0, -3), froggerscene r)]

scenes :: StdGen -> [Scene]
scenes r =
  [startscene (selectionsmenu r) r, endscene r, snakescene r, froggerscene r]

getscene :: Int -> Int -> StdGen -> Scene
getscene t s r = scene {score = s}
  where
    scene = scenes r !! t

getmove :: Event -> Scene -> Scene
getmove e s = move s e s

getupdate :: Float -> Scene -> Scene
getupdate fl scene
  | next scene == (-1) = update scene fl scene
  | otherwise = getscene (next scene) (score scene) (random scene)

initialise :: IO ()
initialise = do
  stdGen <- getStdGen
  play
    (InWindow "PRIMEVAL GAMES" (500, 900) (10, 10))
    backgroundColor
    2
    (head $ scenes stdGen)
    renderframe
    getmove
    getupdate
