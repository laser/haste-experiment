{-# LANGUAGE ScopedTypeVariables#-}

module Main where
import Haste
import Haste.Ajax
import Haste.JSON
import Haste.Prim
import Control.Applicative

main :: IO Bool
main = withElems ["submit", "output"] bindHandlers

bindHandlers :: [Elem] -> IO Bool
bindHandlers [button, textarea] = do
  onEvent button OnClick $ onSubmitClicked textarea

onSubmitClicked :: Elem -> Int -> (Int, Int) -> IO ()
onSubmitClicked outputEl x y = do
  setText outputEl "" -- clear previous output
  jsonRequest GET "http://localhost:3000/api" [] (onApiResponse outputEl)

setText :: Elem -> String -> IO ()
setText el s = setProp el "innerHTML" s

onApiResponse :: Elem -> Maybe JSON -> IO ()
onApiResponse outputEl Nothing = setText outputEl "error obtaining JSON from server"
onApiResponse outputEl (Just json) = case json ~> (toJSStr "message") of
  Just msgJSON -> setText outputEl $ fromJSStr $ encodeJSON msgJSON
  _ -> setText outputEl "error parsing message from JSON"
