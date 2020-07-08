{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Orthography where

import qualified Data.Char as C
import qualified Data.Text as T

(+++) = T.append

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

instance Orthographic T.Text where
  isVowel = T.all isVowel
  isConsonant = T.all isConsonant

lastVowel :: T.Text -> Maybe Char
lastVowel s = case T.filter isVowel s of
                xs | T.null xs -> Nothing
                xs -> Just (T.last xs)

endsWithVowel :: T.Text -> Maybe Char
endsWithVowel s = if isVowel (T.last s) then Just (T.last s) else Nothing

endsWithConsonant :: T.Text -> Maybe Char
endsWithConsonant s = if isConsonant (T.last s) then Just (T.last s) else Nothing

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
shouldBeLenition :: T.Text -> Maybe T.Text
shouldBeLenition s =
  if T.last s `elem` nonContinuant
    then Just $ T.snoc (T.init s) (lenition (T.last s))
    else Nothing

-- | Check if there should be fortitive assimilation (sertleşme)
-- Return the new root if yes.
shouldBeFortition :: T.Text -> Maybe T.Text
shouldBeFortition s =
  if T.last s `elem` ("cdg" :: [Char])
    then Just $ T.snoc (T.init s) (lenition (T.last s))
    else Nothing

needsFortis :: T.Text -> Bool
needsFortis s = T.last s `elem` nonContinuant
