module Main where

import Haste
import Haste.Ajax

main = withElems ["btn", "out"]
  $ \[btn, out] -> onEvent btn OnClick
    $ \_ _ -> textRequest GET "/api" []
      $ \res ->
        case res of
          Just s  -> setProp out "value" s
          Nothing -> setProp out "value" ""
