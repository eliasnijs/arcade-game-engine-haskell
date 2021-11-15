{-

  This is a template file for games in this engine

  After setting up this file, you need to add the scene in 2 places in Manager.hs
    1. in selectionmenu along with the coordinates of button to start the game
    2. at the end of the scenes in case you want this to be accesible from a different scene then 'start' 
  and in 1 place in the engine.cabal file, at the end of the exposed modules of lib

  Additionally, you need to declare a gamemode for the engine in Engine.hs along with any custom variables in it.

-}
module Template
  ( templatescene
  ) where

import Config
import Engine

templatescene :: Scene
templatescene =
  Scene
    move
    update
    startpos
    startfriendlies
    startenemies
    startscore
    customgamemode
    (-1)

move :: Event -> Scene -> Scene
move = undefined

update :: Float -> Scene -> Scene
update _ scene
  | nextcondition scene = scene {next = 1}
  | otherwise = undefined

nextcondition :: Scene -> Bool
nextcondition = undefined

startpos :: Vec2
startpos = undefined

startfriendlies :: [Vec2]
startfriendlies = undefined

startenemies :: [Vec2]
startenemies = undefined

startscore :: Int
startscore = undefined

customgamemode :: Gamemode
customgamemode = undefined
