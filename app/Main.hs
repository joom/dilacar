module Main where

import Lib

main :: IO ()
main = interact (show . entry)
