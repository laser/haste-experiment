module Main where

import Haste

main = withElem "cat" $ \img -> do
  onEvent img OnMouseOver
    $ const
    $ setAttr img "style" "border: 10px solid;"

  onEvent img OnMouseOut
    $ setAttr img "style" "border: none;"
