import Haste.App

main :: IO ()
main = runApp (mkConfig "ws://0.0.0.0:1111" 1111) clientMain

echoServer :: String -> Server ()
echoServer s = liftIO . putStrLn $ "The fox said: " ++ s

clientMain :: App Done
clientMain = do
  tellServer <- remote $ echoServer
  runClient $ do
    name <- prompt "What does the fox say?"
    onServer $ tellServer <.> name
