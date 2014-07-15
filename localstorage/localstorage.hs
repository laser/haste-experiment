module Main where
import Haste
import Haste.LocalStorage
import Control.Applicative

data DemoUI = DemoUI { textarea :: Elem, saveButton :: Elem, loadButton :: Elem }

main :: IO ()
main = withElem "root" runClient

runClient :: Elem -> IO ()
runClient root = do
  buildUI root >>= bindHandlers
  return ()

buildUI :: Elem -> IO DemoUI
buildUI root = do
  [textarea, br, button1, button2] <- mapM newElem ["textarea", "br", "button", "button"]
  [btnTxt1, btnTxt2] <- mapM newTextElem ["load from localStorage", "save to localStorage"]
  setProp textarea "id" "output"
  setProp textarea "cols" "30"
  setProp textarea "rows" "10"
  setProp button1 "id" "load"
  setProp button2 "id" "save"
  setChildren root [textarea, br, button1, button2]
  addChild btnTxt1 button1
  addChild btnTxt2 button2
  return DemoUI { textarea=textarea, saveButton=button2, loadButton=button1 }

bindHandlers :: DemoUI -> IO Bool
bindHandlers ui = do
  onEvent (saveButton ui) OnClick (onSaveClicked $ textarea ui)
  onEvent (loadButton ui) OnClick (onLoadClicked $ textarea ui)

onLoadClicked :: Elem -> Int -> (Int, Int) -> IO ()
onLoadClicked outputEl _ _ = getItem "message" >>= either (const $ return ()) (setText outputEl)

onSaveClicked :: Elem -> Int -> (Int, Int) -> IO ()
onSaveClicked outputEl _ _ = getText outputEl >>= setItem "message"

setText :: Elem -> String -> IO ()
setText el s = setProp el "innerHTML" s

getText :: Elem -> IO (String)
getText el = getProp el "innerHTML"
