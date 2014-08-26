import Haste.App
import Data.IORef

main = runApp (mkConfig "ws://0.0.0.0:24601" 24601) $ do
  counter <- liftServerIO $ newIORef (0 :: Int)
  inc'    <- remote $ increment counter

  runClient $ withElems ["btn", "out"] $ \[btn, out] ->
    onEvent btn OnClick $ \_ _ -> do
      n <- onServer inc'
      setProp out "innerHTML" $ show n

increment :: Server (IORef Int) -> Server Int
increment serverState = do
  ref <- serverState
  liftIO $ atomicModifyIORef ref $ \x -> (x + 1, x)
