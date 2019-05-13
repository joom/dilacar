{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Lokum where

import Data.List

import qualified Orthography as Ortho
import qualified Morphology as Morph
import qualified Ottoman as Otto
import qualified Dictionary as Dict

type Result = [String]

translate :: Otto.OttoText -> Result
translate (Otto.Punctuation s) = [s]
translate (Otto.Word letters) =
  error . show $ Morph.morphParse letters

entry :: String -> [Result]
entry s = case Otto.runParser s of
            Left err -> []
            Right ws -> map translate ws
