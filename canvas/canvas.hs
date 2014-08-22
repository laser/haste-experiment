module Main where

import Haste
import Haste.Graphics.Canvas

square = do
  rect (-40, -40) (40, 40)
  rect (-10, -10) (10, 10)

main = do
  Just canvas <- getCanvasById "canvas"
  animate canvas 0

animate canvas angle = do
  let stroked = stroke square
  let rotated = rotate angle stroked
  let drawn   = translate (70, 70) rotated

  render canvas drawn

  setTimeout 10 $ animate canvas (angle + 0.01)
