{-# LANGUAGE ScopedTypeVariables#-}

module Main where
import Haste
import Haste.Ajax
import Haste.JSON
import Haste.Prim
import Control.Applicative

main :: IO Bool
main = withElems ["submit", "output"] runClient

runClient [button, textarea] = do
  onEvent button OnClick $ \_ _ -> do
    setText ""
    jsonRequest GET "http://localhost:3000/api" [] handleResp
  where
    setText t = setProp textarea "innerHTML" t
    handleResp Nothing = setText "error obtaining JSON from server"
    handleResp (Just json) = case json ~> (toJSStr "message") of
      Just msgJSON -> setText . fromJSStr $ encodeJSON msgJSON
      _ -> setText "error parsing message from JSON"
