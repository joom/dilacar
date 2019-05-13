{-# LANGUAGE ViewPatterns #-}
module Dictionary where

import Data.List
import Data.List.Split
import System.IO
import Text.Regex.Applicative

import Ottoman
import Orthography

mid :: [a] -> [a]
mid = tail . init

lookupOttoman :: [OttoModified] -> IO [String]
lookupOttoman letters = do
    handle <- openFile "ottoDict.csv" ReadMode
    readData [] handle
  where
    -- | Go through the dictionary and collect matches
    readData :: [String] -> Handle -> IO [String]
    readData l handle = do
      isFileEnd <- hIsEOF handle
      if isFileEnd
        then return l
        else do
          line <- hGetLine handle
          case splitOn "," line of
            [mid -> tr, mid -> ot] -> do
              let ot' = case runParser ot of
                          Right [Word x] -> x
                          _ -> []
              readData (if baseEq ot' letters then (tr:l) else l) handle
            _ -> readData l handle

lookupModern :: RE s a -> [String]
lookupModern r = []
