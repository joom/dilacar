module Orthography where

import qualified Data.Char as C

alphabet :: String
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

consonants :: [Char]
consonants = lenis ++ fortis

front :: [Char]
front = "eiöü"

back :: [Char]
back = "aıou"

rounded :: [Char]
rounded = "öüou"

unrounded :: [Char]
unrounded = "eiaı"

vowels :: [Char]
vowels = front ++ back
