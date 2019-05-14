{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Orthography where

import qualified Data.Char as C

alphabet :: [Char]
alphabet = "abcçdefgğhıijklmnoöprsştuüvyz"

toUpper :: Char -> Char
toUpper 'ı' = 'I'
toUpper 'i' = 'İ'
toUpper c   = C.toUpper c

toLower :: Char -> Char
toLower 'I' = 'ı'
toLower 'İ' = 'i'
toLower c = C.toLower c

-- | "yumuşak ünsüzler"
lenis :: [Char]
lenis = "bcdğgjlmnrvyz"

-- | "sürekli ünsüzler"
continuant :: [Char]
continuant = "fhsş"

-- | "süreksiz ünsüzler"
nonContinuant :: [Char]
nonContinuant = "pçtk"

-- | "sert ünsüzler"
fortis :: [Char]
fortis = continuant ++ nonContinuant

-- | "ünsüzler"
consonants :: [Char]
consonants = lenis ++ fortis

-- | "ince ünlüler"
front :: [Char]
front = "eiöüî"

-- | "kalın ünlüler"
back :: [Char]
back = "aıouâ"

-- | "yuvarlak ünlüler"
rounded :: [Char]
rounded = "öüou"

-- | "düz ünlüler"
unrounded :: [Char]
unrounded = "eiaı"

-- | "ünlüler"
vowels :: [Char]
vowels = front ++ back

class Orthographic a where
  isVowel :: a -> Bool
  isConsonant :: a -> Bool

instance Orthographic Char where
  isVowel = (`elem` vowels)
  isConsonant = (`elem` consonants)

instance Orthographic String where
  isVowel = all isVowel
  isConsonant = all isConsonant

lastVowel :: String -> Maybe Char
lastVowel s = case filter isVowel s of
                [] -> Nothing
                xs -> Just (last xs)

endsWithVowel :: String -> Maybe Char
endsWithVowel s = if isVowel (last s) then Just (last s) else Nothing

endsWithConsonant :: String -> Maybe Char
endsWithConsonant s = if isConsonant (last s) then Just (last s) else Nothing

lenition :: Char -> Char
lenition 'p' = 'b'
lenition 'ç' = 'c'
lenition 't' = 'd'
lenition 'k' = 'ğ'

fortition :: Char -> Char
fortition 'c' = 'ç'
fortition 'd' = 't'
fortition 'g' = 'k'

-- | Check if there should be consonant lenition (yumuşama)
-- Return the new root if yes.
shouldBeLenition :: String -> Maybe String
shouldBeLenition s =
  if last s `elem` nonContinuant
    then Just $ init s ++ [lenition (last s)]
    else Nothing

-- | Check if there should be fortitive assimilation (sertleşme)
-- Return the new root if yes.
shouldBeFortition :: String -> Maybe String
shouldBeFortition s =
  if last s `elem` "cdg"
    then Just $ init s ++ [lenition (last s)]
    else Nothing


