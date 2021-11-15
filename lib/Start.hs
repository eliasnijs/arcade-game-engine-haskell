module Start
  ( startscene
  ) where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Engine
import Renderer

startscene :: [(Vec2, Scene)] -> StdGen -> Scene
startscene p =
  Scene startmove startupdate (0, 0) [] [fst e | e <- p] 0 (Start p) (-1)

startupdate :: Float -> Scene -> Scene
startupdate _ (Scene m u (px, py) f e s (Start levels) n t)
  | (px, py) `elem` e =
    snd $ head $ filter (\x -> fst (fst x) == px && snd (fst x) == py) levels
  | otherwise = Scene m u (px, py) f e s (Start levels) n t

startmove :: Event -> Scene -> Scene
startmove (EventKey (SpecialKey KeyUp) Down _ _) scene =
  scene {player = (fst $ player scene, incBound (snd $ player scene) top)}
startmove (EventKey (SpecialKey KeyDown) Down _ _) scene =
  scene {player = (fst $ player scene, decBound (snd $ player scene) bottom)}
startmove (EventKey (SpecialKey KeyRight) Down _ _) scene =
  scene {player = (incBound (fst $ player scene) right, snd $ player scene)}
startmove (EventKey (SpecialKey KeyLeft) Down _ _) scene =
  scene {player = (decBound (fst $ player scene) left, snd $ player scene)}
startmove _ g = g
