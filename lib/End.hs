module End where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Engine
import Start

endscene :: StdGen -> Scene
endscene =
  Scene
    endmove
    endupdate
    (0, 0)
    [(x, bottom) | x <- [left .. right]]
    []
    0
    End
    (-1)

endupdate :: Float -> Scene -> Scene
endupdate _ scene
  | atBottom (player scene) = scene {next = 0}
  | otherwise = scene

endmove :: Event -> Scene -> Scene
endmove (EventKey (SpecialKey KeyUp) Down _ _) scene =
  scene {player = (fst $ player scene, incBound (snd $ player scene) top)}
endmove (EventKey (SpecialKey KeyDown) Down _ _) scene =
  scene {player = (fst $ player scene, decBound (snd $ player scene) bottom)}
endmove (EventKey (SpecialKey KeyRight) Down _ _) scene =
  scene {player = (incBound (fst $ player scene) right, snd $ player scene)}
endmove (EventKey (SpecialKey KeyLeft) Down _ _) scene =
  scene {player = (decBound (fst $ player scene) left, snd $ player scene)}
endmove _ g = g
