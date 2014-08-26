import Haste
import Haste.Foreign

addInt :: Int -> Int -> IO Int
addInt x y = return $ x + y

main :: IO ()
main = do
  export (toJSString "addInt") addInt
