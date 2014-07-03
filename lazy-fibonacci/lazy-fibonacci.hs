{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Haste

-- writeLog

main :: IO ()
main = writeLog $ foldl (\acc n -> acc ++ " " ++ show n) "" $ take 13 fibs

fibs :: [Integer]
fibs = map fibs' [1..]
  where fibs' n
          | n == 1 = 0
          | n == 2 = 1
          | n > 2 = fibs' (n-2) + fibs' (n-1)
