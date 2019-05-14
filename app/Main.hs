module Main where

import Lokum

main :: IO ()
main = do
  s <- getContents
  res <- entry s
  putStrLn $ show res
