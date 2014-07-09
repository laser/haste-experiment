{-# LANGUAGE ScopedTypeVariables#-}

module Main where
import Haste
import Haste.Ajax
import Haste.JSON
import Haste.Prim
import Control.Applicative

data AjaxUI = AjaxUI { textarea :: Elem, button :: Elem }

main :: IO ()
main = withElem "root" runClient

runClient :: Elem -> IO ()
runClient root = do
  ui <- buildUI root
  bindHandlers (button ui) (textarea ui)
  return ()

buildUI :: Elem -> IO AjaxUI
buildUI root = do
  [textarea, br, button] <- mapM newElem ["textarea", "br", "button"]
  clickMe <- newTextElem "Ajax it up!"
  setProp textarea "id" "output"
  setProp textarea "cols" "30"
  setProp textarea "rows" "10"
  setProp button "id" "submit"
  setChildren root [textarea, br, button]
  addChild clickMe button
  return AjaxUI { textarea=textarea, button=button }

bindHandlers :: Elem -> Elem -> IO Bool
bindHandlers button textarea = do
  onEvent button OnClick $ onSubmitClicked textarea

onSubmitClicked :: Elem -> Int -> (Int, Int) -> IO ()
onSubmitClicked outputEl x y = do
  setText outputEl "" -- clear previous output
  jsonRequest GET "http://localhost:3000/api" [] (onApiResponse outputEl)

onApiResponse :: Elem -> Maybe JSON -> IO ()
onApiResponse outputEl Nothing = setText outputEl "error obtaining JSON from server"
onApiResponse outputEl (Just json) = case json ~> (toJSStr "message") of
  Just msgJSON -> setText outputEl $ fromJSStr $ encodeJSON msgJSON
  _ -> setText outputEl "error parsing message from JSON"

setText :: Elem -> String -> IO ()
setText el s = setProp el "innerHTML" s
