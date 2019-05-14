{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Ottoman where

import Data.List
import Data.List.Extra

import Text.Parsec.String (Parser(..))
import Text.Parsec ((<|>), string, many, many1, spaces)
import qualified Text.Parsec

class Parse a where
  parse :: Parser a

data Otto =
    Elif
  | Be
  | Pe
  | Te
  | Se
  | Cim
  | Çim
  | Ha
  | Hı
  | Dal
  | Zel
  | Re
  | Ze
  | Je
  | Sin
  | Şın
  | Sad
  | Dad
  | Tı
  | Zı
  | Ayn
  | Gayn
  | Fe
  | Kaf
  | Kef
  | Gef
  | Nef
  | Lam
  | Mim
  | Nun
  | Vav
  | He
  | Ye
  deriving (Show, Eq, Enum)

instance Parse Otto where
  parse = (string "ا" *> pure Elif)
      <|> (string "ب" *> pure Be)
      <|> (string "پ" *> pure Pe)
      <|> (string "ت" *> pure Te)
      <|> (string "ث" *> pure Se)
      <|> (string "ج" *> pure Cim)
      <|> (string "چ" *> pure Çim)
      <|> (string "ح" *> pure Ha)
      <|> (string "خ" *> pure Hı)
      <|> (string "د" *> pure Dal)
      <|> (string "ذ" *> pure Zel)
      <|> (string "ر" *> pure Re)
      <|> (string "ز" *> pure Ze)
      <|> (string "ژ" *> pure Je)
      <|> (string "س" *> pure Sin)
      <|> (string "ش" *> pure Şın)
      <|> (string "ص" *> pure Sad)
      <|> (string "ض" *> pure Dad)
      <|> (string "ط" *> pure Tı)
      <|> (string "ظ" *> pure Zı)
      <|> (string "ع" *> pure Ayn)
      <|> (string "غ" *> pure Gayn)
      <|> (string "ف" *> pure Fe)
      <|> (string "ق" *> pure Kaf)
      <|> (string "ك" *> pure Kef)
      <|> (string "گ" *> pure Gef)
      <|> (string "ڭ" *> pure Nef)
      <|> (string "ل" *> pure Lam)
      <|> (string "م" *> pure Mim)
      <|> (string "ن" *> pure Nun)
      <|> (string "و" *> pure Vav)
      <|> (string "ه" *> pure He)
      <|> (string "ی" *> pure Ye)
      <|> (string "ي" *> pure Ye)

data Mod =
    Hemze
  | Üstün
  | Esre
  | Ötre
  | Şedde
  | Sükun
  | Medde
  | Fethateyn -- ^ Tenvin: also known as "iki üstün"
  | Kesrateyn -- ^ Tenvin: also known as "iki esre"
  | Dammeteyn -- ^ Tenvin: also known as "iki ötre"
  deriving (Show, Eq, Enum)

instance Parse Mod where
  parse = (string "\1611" *> pure Fethateyn)
      <|> (string "\1612" *> pure Kesrateyn)
      <|> (string "\1613" *> pure Dammeteyn)
      <|> (string "\1614" *> pure Üstün)
      <|> (string "\1615" *> pure Ötre)
      <|> (string "\1616" *> pure Esre)
      <|> (string "\1617" *> pure Şedde)
      <|> (string "\1618" *> pure Sükun)
      <|> (string "\1619" *> pure Medde) -- normal size
      <|> (string "\1620" *> pure Hemze) -- above
      <|> (string "\1621" *> pure Hemze) -- below
      <|> (string "\1764" *> pure Medde) -- small high

data OttoModified =
    PureOtto Otto
  | ModifiedOtto [Mod] Otto
  deriving (Show, Eq)

baseOtto :: OttoModified -> Otto
baseOtto (PureOtto x) = x
baseOtto (ModifiedOtto _ x) = x

endsWith :: [OttoModified] -> [Otto] -> Maybe [OttoModified]
endsWith xs ys = (map fst . zip xs) <$> (stripSuffix ys (map baseOtto xs))

instance Parse OttoModified where
  parse =
    -- Exception cases for the special Unicode characters
    -- that stand for an Ottoman leter with a modification
          (string "\1570" *> pure (ModifiedOtto [Medde] Elif))
      <|> (string "\1571" *> pure (ModifiedOtto [Hemze] Elif)) -- Hemze above elif
      <|> (string "\1573" *> pure (ModifiedOtto [Hemze] Elif)) -- Hemze below elif
      <|> (do otto <- parse @Otto
              mods <- many (parse @Mod)
              pure (ModifiedOtto mods otto))

data OttoText =
    Word [OttoModified]
  | Punctuation String
  deriving (Show, Eq)

baseEq :: [OttoModified] -> [OttoModified] -> Bool
baseEq xs ys = (map baseOtto xs) == (map baseOtto ys)

instance Parse OttoText where
  parse = (Word <$> (spaces *> many1 (parse @OttoModified) <* spaces))
      <|> (Punctuation <$> (string "." <|> string ","))

runParser :: String -> Either Text.Parsec.ParseError [OttoText]
runParser = Text.Parsec.parse (many parse) ""
