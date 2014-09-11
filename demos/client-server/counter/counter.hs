import Haste.App
import Data.IORef

main :: IO ()
main = runApp (mkConfig "ws://0.0.0.0:1111" 1111) $ do
  useless <- liftServerIO $ newIORef (0 :: Int)

  increment <- remote $ do
    ref <- useless
    liftIO $ atomicModifyIORef ref $ (\x -> (x+1, x+1))

  runClient $ do
    clients <- onServer increment
    alert $ "You are visitor #" ++ show clients
