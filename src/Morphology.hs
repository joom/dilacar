{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Morphology where

import Data.List

import Orthography
import Ottoman

data Suffix =
    Ler -- ler, lar
  | Den -- den, dan, ten, tan
  | In -- in, ın, un, ün
  | Di -- di, dı, du, dü, ti, tı, tu, tü
  | Miş -- miş, mış, muş, müş
  | Ecek -- ecek, acak
  deriving (Show, Eq, Enum, Bounded)
  -- TODO a lot more suffixes to come!

allSuffixes :: [Suffix]
allSuffixes = [minBound .. maxBound]

addSuffix :: String -> Suffix -> String
addSuffix w sf = undefined

suffixOttoman :: Suffix -> [[Otto]]
suffixOttoman = \case
  Ler -> [[Lam, Re]]
  Den -> [[Dal, Nun]]
  In -> [[Kef], [Nef]]
  Di -> [[Dal, Ye]]
  Miş -> [[Mim, Şın]]
  Ecek -> [[Cim, Kef], [Cim, Kaf]]

morphParse :: [OttoModified] -> ([OttoModified], [Suffix])
morphParse ((`endsWith` [Lam, Re])  -> Just x) = (x, [Ler])
morphParse ((`endsWith` [Dal, Nun]) -> Just x) = (x, [Den])
morphParse ((`endsWith` [Kef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Nef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Dal, Ye])  -> Just x) = (x, [Di])
morphParse ((`endsWith` [Mim, Şın]) -> Just x) = (x, [Miş])
morphParse ((`endsWith` [Cim, Kef]) -> Just x) = (x, [Ecek])
morphParse ((`endsWith` [Cim, Kaf]) -> Just x) = (x, [Ecek])
morphParse letters = (letters, [])

reconstruct :: [Suffix] -> String -> String
reconstruct (x : xs) root = reconstruct xs (reconstructOne x root)
reconstruct [] root = root

reconstructOne :: Suffix -> String -> String
reconstructOne Ler root =
  case (`elem` front) <$> lastVowel root of
    Just False -> root ++ "lar"
    _ -> root ++ "ler"
reconstructOne Den root =
  let isFront = (`elem` front) <$> lastVowel root in
  case (isFront, needsFortis root) of
    (Just False, True) -> root ++ "tan"
    (Just False, False) -> root ++ "dan"
    (_, True) -> root ++ "ten"
    (_, False) -> root ++ "den"
reconstructOne Miş root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ "mış"
    (Just False, Just True) -> root ++ "miş"
    (Just True, Just False) -> root ++ "muş"
    (Just True, Just True) -> root ++ "müş"
reconstructOne Di root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront, needsFortis root) of
    (Just False, Just False, True) -> root ++ "tı"
    (Just False, Just False, False) -> root ++ "dı"
    (Just False, Just True, True) -> root ++ "ti"
    (Just False, Just True, False) -> root ++ "di"
    (Just True, Just False, True) -> root ++ "tu"
    (Just True, Just False, False) -> root ++ "du"
    (Just True, Just True, True) -> root ++ "tü"
    (Just True, Just True, False) -> root ++ "dü"
reconstructOne _ root = error "Unimplemented suffix" -- TODO
