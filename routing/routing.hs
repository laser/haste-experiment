module Main where

import Haste

setText el msg = do
  let msg' = if null msg
             then "hello"
             else msg

  setProp el "innerHTML" msg'

main = withElem "message" $ \el -> do
  hash <- getHash
  setText el hash

  onHashChange $ \old new ->
    setText el new
