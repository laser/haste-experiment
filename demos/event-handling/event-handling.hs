module Main where

import Haste

main = withElem "root" $ \root -> do
  img <- newElem "img"

  setAttr img "src" "cat.jpg"
  setAttr img "id" "cat"
  addChild img root

  onEvent img OnMouseOver $ \_ ->
    setClass img "foo" True

  onEvent img OnMouseOut
    $ setClass img "foo" False
