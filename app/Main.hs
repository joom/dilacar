{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Dilacar

main :: IO ()
main = T.getContents >>= run

run :: T.Text -> IO ()
run s = do
  res <- entry s
  -- putStrLn $ intercalate "\n" (map (intercalate ", ") res)
  T.putStrLn $ T.intercalate "\n" (map (T.intercalate ", ") res)
