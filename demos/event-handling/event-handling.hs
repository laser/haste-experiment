module Main where

import Haste

main = withElem "cat"
  $ \img -> do
    onEvent img OnMouseOver
      $ \_ ->
        setClass img "foo" True

    onEvent img OnMouseOut
      $ setClass img "foo" False
