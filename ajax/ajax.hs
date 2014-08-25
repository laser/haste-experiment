module Main where

import Haste
import Haste.Ajax

main = withElems ["btn", "out"] app

app [btn, out] = do
  onEvent btn OnClick $ \_ _ -> do
    textRequest GET "/api" [] $ \mstr ->
      case mstr of
        Just s  -> txt out s
        Nothing -> txt out ""

txt el s = setProp el "innerHTML" s
