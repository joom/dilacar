{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
morphParse ((`endsWith` [Lam, Re])  -> Just x) = (x, [Ler])
morphParse ((`endsWith` [Kef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Nef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Dal, Ye])  -> Just x) = (x, [Di])
morphParse ((`endsWith` [Mim, Şın]) -> Just x) = (x, [Miş])
morphParse ((`endsWith` [Cim, Kef]) -> Just x) = (x, [Ecek])
morphParse ((`endsWith` [Cim, Kaf]) -> Just x) = (x, [Ecek])
morphParse letters = (letters, [])
