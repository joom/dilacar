{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Morphology where

import Data.List

import Ottoman

data Suffix =
    Ler -- ler, lar
  | In -- in, ın, un, ün
  | Di -- di, dı, du, dü, ti, tı, tu, tü
  | Miş -- miş, mış, muş, müş
  | Ecek -- ecek, acak
  deriving (Show, Eq, Enum, Bounded)

allSuffixes :: [Suffix]
allSuffixes = [minBound .. maxBound]

addSuffix :: String -> Suffix -> String
addSuffix w sf = undefined

suffixOttoman :: Suffix -> [[Otto]]
suffixOttoman = \case
  Ler -> [[Lam, Re]]
  In -> [[Kef], [Nef]]
  Di -> [[Dal, Ye]]
  Miş -> [[Mim, Şın]]
  Ecek -> [[Cim, Kef], [Cim, Kaf]]

morphParse :: [OttoModified] -> ([OttoModified], [Suffix])
morphParse letters =
  undefined
    -- case find (any (`isSuffixOf` letters) . suffixOttoman) allSuffixes of
    --   Just suf -> (letters, [suf])
    --   Nothing -> (letters, [])
