module Renderer
  ( renderframe
  ) where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Config
import Engine

gridToviewCoords :: Vec2 -> (Float, Float)
gridToviewCoords (x, y) =
  (fromIntegral x * dblock * fscale, fromIntegral y * dblock * fscale)

drawPixel :: Color -> Vec2 -> Picture
drawPixel c p =
  translate vc1 vc2 (scale (fscale * dwidth) (fscale * dwidth) pixel)
  where
    (vc1, vc2) = gridToviewCoords p
    pixel =
      Color c $
      pictures
        [rectangleWire 1 1, rectangleSolid (dinner / dwidth) (dinner / dwidth)]

drawfield :: Picture
drawfield =
  pictures $
  drawPixel emptyColor <$> [(x, y) | x <- [left .. right], y <- [bottom .. top]]

renderframe :: Scene -> Picture
renderframe (Scene _ _ p f e s End _ _) =
  pictures
    [ drawfield
    , pictures $ fmap (drawPixel enemyColor) e
    , pictures $ fmap (drawPixel friendlyColor) f
    , color friendlyColor $ text $ show s
    , drawPixel playerColor p
    ]
renderframe scene =
  pictures
    [ drawfield
    , pictures $ fmap (drawPixel enemyColor) (enemies scene)
    , pictures $ fmap (drawPixel friendlyColor) (friendlies scene)
    , drawPixel playerColor (player scene)
    ]
