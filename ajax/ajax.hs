module Main where
import Haste
import Haste.Ajax

main :: IO Bool
main = withElems ["submit", "output"] ajaxIfy

ajaxIfy [button, textarea] = do
  onEvent button OnClick $ \_ _ -> do
    setText ""
    textRequest GET "http://localhost:3000/api" [] $ \resp -> do
      case resp of
        Just text -> setText text
        _      -> return ()
  where
    setText t = setProp textarea "innerHTML" t
