import Haste

foreign import ccall add :: Int -> Int -> IO (Int)

main = withElems ["compute", "a", "b", "output"] app

app [compute, a, b, output] = do
  onEvent compute OnClick $ \_ _ -> do
    a1  <- getProp a "value"
    b1  <- getProp b "value"
    sum <- add (read a1) (read b1)
    setProp output "value" $ show sum
