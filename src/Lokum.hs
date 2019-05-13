{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Lokum where

import Orthography
import Morphology
import Ottoman

type Result = [String]

word :: String -> Result
word w = undefined

entry :: String -> [Result]
entry s = map word (words s)
