{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Pattern where

import Text.Regex.Applicative

import Ottoman
import Orthography

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

pat :: OttoModified -> RE Char String
pat (PureOtto base) = foldr1 (<|>) (map string opts)
  where opts = ottoPat base
        cons = filter isConsonant opts
        vows = filter isVowel opts
pat (ModifiedOtto mods base) = foldr1 (<|>) (map string opts)
  where opts = ottoPat base
        cons = filter isConsonant opts
        vows = filter isVowel opts

cat :: Semigroup a => RE Char a -> RE Char a -> RE Char a
cat x y = pure (<>) <*> x <*> y

generateRegex :: [OttoModified] -> RE Char String
generateRegex = go True
  where
    go :: Bool -> [OttoModified] -> RE Char String
    -- Elif with Medde in the beginning of a word is "a"
    go True (ModifiedOtto mods Elif : rest) | Medde `elem` mods =
       string "a" `cat` go False rest
    -- Elif with a following vowelish Ottoman letter is probably a vowel
    go True letters@((baseOtto -> Elif) : (baseOtto -> following) : rest) =
          alt (filter isVowel (ottoPat following)) `cat` go False rest
      <|> go False letters
    go isBeginning letters = foldr1 cat (map pat letters)
