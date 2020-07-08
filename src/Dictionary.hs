{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Dictionary where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Text.Regex.Applicative

import Ottoman
import Orthography

-- | Used for getting rid of quotation marks.
mid :: T.Text -> T.Text
mid = T.tail . T.init

lookupOttoman :: [OttoModified] -> IO [T.Text]
lookupOttoman letters = do
    handle <- openFile "ottoDict.csv" ReadMode
    readData [] handle
  where
    -- | Go through the dictionary and collect matches
    readData :: [T.Text] -> Handle -> IO [T.Text]
    readData l handle = do
      isFileEnd <- hIsEOF handle
      if isFileEnd
        then return l
        else do
          line <- T.hGetLine handle
          case T.splitOn "," line of
            [mid -> tr, mid -> ot] -> do
              let ot' = case runParser ot of
                          Right [Word x] -> x
                          _ -> []
              readData (if baseEq ot' letters then (tr:l) else l) handle
            _ -> readData l handle

lookupModern :: RE Char a -> IO [T.Text]
lookupModern r = do
    handle <- openFile "turkDict.csv" ReadMode
    readData [] handle
  where
    -- | Go through the dictionary and collect matches
    readData :: [T.Text] -> Handle -> IO [T.Text]
    readData l handle = do
      isFileEnd <- hIsEOF handle
      if isFileEnd
        then return l
        else do
          line <- T.hGetLine handle
          case (T.unpack line) =~ r of
            Nothing -> readData l handle
            Just _ -> readData (line:l) handle

