module Main where

import Haste

main = withElem "root" 
  $ \root -> do
    marq <- newElem "marquee"
    txt  <- newTextElem "Mark"

    setAttr marq "id" "banner"

    addChild txt marq
    addChild marq root
