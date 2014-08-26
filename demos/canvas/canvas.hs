module Main where

import Haste
import Haste.Graphics.Canvas

main = do
  Just canvas <- getCanvasById "canvas"
  animate canvas 0

square = do
  rect (-40, -40) (40, 40)
  rect (-10, -10) (10, 10)

animate canvas angle = do
  render canvas
    $ translate (70, 70)
    $ rotate angle
    $ stroke square

  setTimeout 10
    $ animate canvas (angle + 0.01)
