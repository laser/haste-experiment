module Main where
import Haste

main :: IO Bool
main = withElems ["submit", "output"] helloWorld

helloWorld [button, textarea] = do
  onEvent button OnClick $ \_ _ -> do
    setProp textarea "innerHTML" $ toString "setTimeout invoked..."
    setTimeout 1000 $ setProp textarea "innerHTML" $ toString "Hello, World!"
