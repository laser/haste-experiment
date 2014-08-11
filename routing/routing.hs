module Main where

import Haste

data DemoUI = DemoUI {
            hello :: Elem,
            goodbye :: Elem,
            helloLink :: Elem,
            goodbyeLink :: Elem
            }

main :: IO ()
main = withElem "root" $ \root -> do
  ui <- buildUI root
  bindHandlers ui
  getHash >>= reveal ui

bindHandlers :: DemoUI -> IO ()
bindHandlers ui = do
  onEvent (helloLink ui) OnClick $ const . const $ setHash "hello"
  onEvent (goodbyeLink ui) OnClick $ const . const $ setHash "goodbye"
  onHashChange $ const $ reveal ui

reveal :: DemoUI -> String -> IO ()
reveal ui which = let pair = if which == "goodbye" then ((goodbye ui), (hello ui))
                                                   else ((hello ui), (goodbye ui))
                  in show' (fst pair) >> hide' (snd pair)

show' :: Elem -> IO ()
show' el = setStyle el "display" "block"

hide' :: Elem -> IO ()
hide' el = setStyle el "display" "none"

buildUI :: Elem -> IO DemoUI
buildUI root = do
  -- create the content-containers
  [hello, goodbye]       <- mapM newElem ["div", "div"]
  [helloTxt, goodbyeTxt] <- mapM newTextElem ["Hello!", "Goodbye!"]
  addChild helloTxt hello
  addChild goodbyeTxt goodbye

  -- create the hyperlinks
  [txt2, txt3]           <- mapM newTextElem ["show hello", "show goodbye"]
  [helloLink, goodbyeLink]     <- mapM newElem ["a", "a"]
  addChild txt2 helloLink
  addChild txt3 goodbyeLink
  setAttr helloLink "href" "#hello"
  setAttr goodbyeLink "href" "#goodbye"

  -- jam everything into the DOM
  setChildren root [hello, goodbye, helloLink, goodbyeLink]

  -- return something with accessors for important elements
  return DemoUI { hello = hello, goodbye = goodbye, helloLink = helloLink, goodbyeLink = goodbyeLink }
