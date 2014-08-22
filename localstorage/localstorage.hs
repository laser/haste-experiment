module Main where

import Haste
import Haste.LocalStorage

main = withElems ["in", "sv", "ld"] $
  \[input, save, load] -> do

    onEvent save OnClick $ \_ _ -> do
      message <- getProp input "value"
      setItem "msg" message

    onEvent load OnClick $ \_ _ -> do
      msg <- getItem "msg"
      either
        (\_ -> return ())
        (setProp input "value")
        msg
