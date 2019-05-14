{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Lokum where

import Data.List

import qualified Orthography as Ortho
import qualified Morphology as Morph
import qualified Ottoman as Otto
import qualified Pattern as Pat
import qualified Dictionary as Dict

type Result = [String]

translate :: Otto.OttoText -> IO Result
translate (Otto.Punctuation s) = return [s]
translate (Otto.Word letters) = loop letters []
  where
    loop :: [Otto.OttoModified] -> [Morph.Suffix] -> IO Result
    loop letters accSuffix =
      -- Partial morphological parsing from the Ottoman letters
      case Morph.morphParse letters of
        (presumptiveRoot, sufs) ->
          let allSufs = sufs ++ accSuffix in
          -- Look up in the Ottoman dictionary with the presumptive root
          Dict.lookupOttoman presumptiveRoot >>= \case
            translatedRoots@(_:_) ->
              -- If there's a translation, reconstruct all with the suffixes
              return $ map (Morph.reconstruct allSufs) translatedRoots
            -- If there's no Ottoman match, generate regex and look up in modern dictionary
            [] -> Dict.lookupModern (Pat.generateRegex presumptiveRoot) >>= \case
                    translatedRoots@(_:_) ->
                      -- If there's any match, reconstruct all with the suffixes
                      return $ map (Morph.reconstruct allSufs) translatedRoots
                    -- If there's no match, try to parse one more suffix
                    -- TODO report that translation failed
                    [] -> if null sufs then return [] else loop presumptiveRoot allSufs

entry :: String -> IO [Result]
entry s = case Otto.runParser s of
            Left err -> return []
            Right ws -> mapM translate ws
