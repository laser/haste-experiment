import Haste
import Haste.Foreign

addInt :: Int -> Int -> IO Int
addInt x y = return $ x + y

main :: IO ()
main = withElem "bar"
  $ \el ->
      alert "yarp!"
  {-export (toJSString "addInt") addInt-}


