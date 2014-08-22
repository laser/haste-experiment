module Main where

import Haste

main = withElem "cat" $ \img -> do
  onEvent img OnMouseOver $ \_ -> do
    setAttr img "style" "border: 10px solid;"

  onEvent img OnMouseOut $ do
    setAttr img "style" "border: none;"
