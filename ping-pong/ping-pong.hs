module Main where

import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C
import Control.Monad
import Control.Applicative
import Data.List (lookup)
import Data.IORef
import qualified Data.Set as S

respond newmsg = liftIO $ newmsg

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    respond <- export respond

    runClient $ withElems ["submit", "output"] $ \[button, textarea] -> do
      onEvent button OnClick $ \_ _ -> do
        newmsg   <- prompt "Enter a message"
        response <- onServer $ respond <.> newmsg
        curr     <- getProp textarea "innerHTML"
        setProp textarea "innerHTML" $ curr ++ toString response ++ "\n"
