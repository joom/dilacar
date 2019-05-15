{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Morphology where

import Data.List

import Orthography
import Ottoman

data Suffix =
  -- declension
    Ler -- ler, lar
  | Den -- den, dan, ten, tan
  | In -- in, ın, un, ün
  -- conjugation
  | Di -- di, dı, du, dü, ti, tı, tu, tü
  | Miş -- miş, mış, muş, müş
  | Ecek -- ecek, acak
  -- derivative
  | Mek -- mek, mak
  | Li -- li, lı
  | Siz -- siz, sız, süz, suz
  | Lik -- lik, lık, lük, luk
  | Leş -- leş, laş
  | Cesine -- cesine, casına, çesine, çasına
  deriving (Show, Eq, Enum, Bounded)
  -- TODO a lot more suffixes to come!

allSuffixes :: [Suffix]
allSuffixes = [minBound .. maxBound]

morphParse :: [OttoModified] -> ([OttoModified], [Suffix])
morphParse ((`endsWith` [Lam, Re])  -> Just x) = (x, [Ler])
morphParse ((`endsWith` [Dal, Nun]) -> Just x) = (x, [Den])
morphParse ((`endsWith` [Kef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Nef])      -> Just x) = (x, [In])
morphParse ((`endsWith` [Dal, Ye])  -> Just x) = (x, [Di])
morphParse ((`endsWith` [Mim, Şın]) -> Just x) = (x, [Miş])
morphParse ((`endsWith` [Cim, Kef]) -> Just x) = (x, [Ecek])
morphParse ((`endsWith` [Cim, Kaf]) -> Just x) = (x, [Ecek])
morphParse ((`endsWith` [Mim, Kef])  -> Just x) = (x, [Mek])
morphParse ((`endsWith` [Mim, Kaf])  -> Just x) = (x, [Mek])
morphParse ((`endsWith` [Lam, Ye])  -> Just x) = (x, [Li])
morphParse ((`endsWith` [Sin, Ze])  -> Just x) = (x, [Siz])
morphParse ((`endsWith` [Lam, Kef])  -> Just x) = (x, [Lik])
morphParse ((`endsWith` [Lam, Kaf])  -> Just x) = (x, [Lik])
morphParse ((`endsWith` [Lam, Şın])  -> Just x) = (x, [Leş])
morphParse letters = (letters, [])

reconstruct :: [Suffix] -> String -> String
reconstruct (x : xs) root = reconstruct xs (reconstructOne x root)
reconstruct [] root = root

-- | "kaynaştırma"
supportive :: Suffix -> String -> String
supportive In "su" = "y"
supportive In _ = "n"
supportive _ _ = "y"

reconstructOne :: Suffix -> String -> String
reconstructOne Ler root =
  let isFront = (`elem` front) <$> lastVowel root in
  case isFront of
    Just False -> root ++ "lar"
    _          -> root ++ "ler"
reconstructOne Den root =
  let isFront = (`elem` front) <$> lastVowel root in
  case (isFront, needsFortis root) of
    (Just False, True)  -> root ++ "tan"
    (Just False, False) -> root ++ "dan"
    (_, True)           -> root ++ "ten"
    (_, False)          -> root ++ "den"
-- TODO supportive consonants written in the Ottoman form aren't handled yet
reconstructOne In root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  let sup = case endsWithVowel root of {Nothing -> "" ; _ -> supportive In root} in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ sup ++ "ın"
    (Just False, Just True)  -> root ++ sup ++ "in"
    (Just True, Just False)  -> root ++ sup ++ "un"
    (Just True, Just True)   -> root ++ sup ++ "ün"
reconstructOne Miş root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ "mış"
    (Just False, Just True)  -> root ++ "miş"
    (Just True, Just False)  -> root ++ "muş"
    (Just True, Just True)   -> root ++ "müş"
reconstructOne Di root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront, needsFortis root) of
    (Just False, Just False, True)  -> root ++ "tı"
    (Just False, Just False, False) -> root ++ "dı"
    (Just False, Just True, True)   -> root ++ "ti"
    (Just False, Just True, False)  -> root ++ "di"
    (Just True, Just False, True)   -> root ++ "tu"
    (Just True, Just False, False)  -> root ++ "du"
    (Just True, Just True, True)    -> root ++ "tü"
    (Just True, Just True, False)   -> root ++ "dü"
reconstructOne Mek root =
  let isFront = (`elem` front) <$> lastVowel root in
  case isFront of
    Just False -> root ++ "mak"
    _          -> root ++ "mek"
reconstructOne Li root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ "lı"
    (Just False, Just True)  -> root ++ "li"
    (Just True, Just False)  -> root ++ "lu"
    (Just True, Just True)   -> root ++ "lü"
reconstructOne Siz root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ "sız"
    (Just False, Just True)  -> root ++ "siz"
    (Just True, Just False)  -> root ++ "suz"
    (Just True, Just True)   -> root ++ "süz"
reconstructOne Lik root =
  let isFront = (`elem` front) <$> lastVowel root in
  let isRounded = (`elem` rounded) <$> lastVowel root in
  case (isRounded, isFront) of
    (Just False, Just False) -> root ++ "lık"
    (Just False, Just True)  -> root ++ "lik"
    (Just True, Just False)  -> root ++ "luk"
    (Just True, Just True)   -> root ++ "lük"
reconstructOne Leş root =
  let isFront = (`elem` front) <$> lastVowel root in
  case isFront of
    Just False -> root ++ "laş"
    _          -> root ++ "leş"
reconstructOne Cesine root =
  let isFront = (`elem` front) <$> lastVowel root in
  case (isFront, needsFortis root) of
    (Just False, False) -> root ++ "casına"
    (_, False)          -> root ++ "cesine"
    (Just False, True)  -> root ++ "çasına"
    (_, True)           -> root ++ "çesine"
reconstructOne _ root = error "Unimplemented suffix" -- TODO
