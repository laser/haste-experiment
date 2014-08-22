module Main where

import Prelude hiding (div)
import Haste
import Haste.Perch

main :: IO ()
main = do
  withElem "main" $ build $ do
    div $ do
      addEvent this OnClick $ \_ _ -> alert "sup players"
      div $ do
        p "hello"
        p ! atr "style" "color:red" $   "world"
  return ()
