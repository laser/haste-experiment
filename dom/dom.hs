module Main where

import Haste

main = withElem "root" $ \root -> do
  marq <- newElem "marquee"
  txt  <- newTextElem "Howdy!"

  setAttr marq "id" "marq"

  addChild txt marq
  addChild marq root
