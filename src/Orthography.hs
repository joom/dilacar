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
lenis = "bcdgğjlmnrvyz"

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
front = "eiöü"

-- | "kalın ünlüler"
back :: [Char]
back = "aıou"

-- | "yuvarlak ünlüler"
rounded :: [Char]
rounded = "öüou"

-- | "düz ünlüler"
unrounded :: [Char]
unrounded = "eiaı"

-- | "ünlüler"
vowels :: [Char]
vowels = front ++ back
