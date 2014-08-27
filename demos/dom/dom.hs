module Main where

import Haste

main = withElem "root"
  $ \root -> do
    marq <- newElem "marquee"
    txt  <- newTextElem "Mark"

    setAttr marq "id" "banner"
    setAttr marq "scrollamount" "20"

    addChild txt marq
    addChild marq root
