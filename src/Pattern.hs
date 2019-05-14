{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Pattern where

import Text.Regex.Applicative

import Ottoman
import Orthography

instance Semigroup a => Semigroup (RE Char a) where
  x <> y = pure (<>) <*> x <*> y

ottoPat :: Otto -> [String]
ottoPat = \case
  Elif -> ["a"]
  Be -> ["b"]
  Pe -> ["p"]
  Te -> ["t"]
  Se -> ["s"]
  Cim -> ["c"]
  Çim -> ["ç"]
  Ha -> ["h"]
  Hı -> ["h"]
  Dal -> ["d"]
  Zel -> ["z"]
  Re -> ["r"]
  Ze -> ["z"]
  Je -> ["j"]
  Sin -> ["s"]
  Şın -> ["ş"]
  Sad -> ["s"]
  Dad -> ["d"]
  Tı -> ["t"]
  Zı -> ["z"]
  Ayn -> []
  Gayn -> ["g", "ğ"]
  Fe -> ["f"]
  Kaf -> ["k"]
  Kef -> ["k"]
  Gef -> ["g"]
  Nef -> ["n"]
  Lam -> ["l"]
  Mim -> ["m"]
  Nun -> ["n"]
  Vav -> ["v", "o", "ö", "u", "ü"]
  He -> ["h", "e", "a"]
  Ye -> ["y", "i", "ı"]

alt :: [String] -> RE Char String
alt xs = foldr1 (<|>) (map string xs)

alt' :: [Char] -> RE Char String
alt' xs = foldr1 (<|>) (map (string . (:[])) xs)

opt x = x <|> string ""
anyVowel = alt' vowels

pat :: OttoModified -> RE Char String
pat (PureOtto base) = pat (ModifiedOtto [] base)
pat (ModifiedOtto mods base) = cons <|> vows
  where opts = ottoPat base
        cons = foldr (\x acc -> (opt anyVowel <> x <> opt anyVowel) <|> acc)
                     empty (map string (filter isConsonant opts))
        vows = foldr (<|>) empty (map string (filter isVowel opts))
        fin = cons <|> vows

generateRegex :: [OttoModified] -> RE Char String
generateRegex = go True
  where
    go :: Bool -> [OttoModified] -> RE Char String
    -- Elif with Medde in the beginning of a word is "a"
    go True (ModifiedOtto mods Elif : rest) | Medde `elem` mods =
       string "a" <> go False rest
    -- Elif with a following vowelish Ottoman letter is probably a vowel
    go True letters@((baseOtto -> Elif) : (baseOtto -> following) : rest) =
          alt (filter isVowel (ottoPat following)) <> go False rest
          -- or not
      <|> go False letters
    go isBeginning letters = foldr1 (<>) (map pat letters)
