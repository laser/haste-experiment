module Main where

import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C
import Control.Monad
import Control.Applicative
import Data.List (lookup)
import Data.IORef
import qualified Data.Set as S

respond :: MonadIO m => String -> m String
respond msg = liftIO $ do
  return ("pong: " ++ msg)

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    respond <- remote respond

    runClient $ withElems ["submit", "output"] $ \[button, textarea] -> do
      onEvent button OnClick $ \_ _ -> do
        msg      <- prompt "ping: "
        response <- onServer $ respond <.> msg
        curr     <- getProp textarea "innerHTML"
        setProp textarea "innerHTML" $ curr ++ toString response ++ "\n"
