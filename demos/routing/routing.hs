module Main where

import Haste

main = withElem "message"
  $ \el -> do
    hash <- getHash
    let msg = if null hash then "hello"
              else hash
    setProp el "value" msg

    onHashChange
      $ \_ msg ->
        setProp el "value" msg
