module Main where

import Haste

main = withElem "cat"
  $ \img -> do
    onEvent img OnMouseOver
      $ const
      $ setAttr img "border" "10"

    onEvent img OnMouseOut
      $ setAttr img "border" "0"
