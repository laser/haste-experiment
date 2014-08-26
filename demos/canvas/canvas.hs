module Main where

import Haste
import Haste.Graphics.Canvas

main = do
  Just canvas <- getCanvasById "canvas"
  animate canvas 0

square = do
  rect (-120, -120) (120, 120)
  rect (-30, -30) (30, 30)

animate canvas angle = do
  render canvas
    $ translate (190, 190)
    $ rotate angle
    $ stroke square

  setTimeout 10
    $ animate canvas (angle + 0.01)
