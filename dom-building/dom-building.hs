{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Haste
import Haste.Perch

-- writeLog

main :: IO ()
main = withElem "root" $ build $ p "hello playerzzz"
