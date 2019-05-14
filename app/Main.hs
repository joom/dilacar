module Main where

import Data.List

import Dilacar

main :: IO ()
main = getContents >>= run

run :: String -> IO ()
run s = do
  res <- entry s
  putStrLn $ intercalate "\n" (map (intercalate ", ") res)
