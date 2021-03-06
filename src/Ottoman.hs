{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Ottoman where

import Data.List.Extra

import qualified Data.Text as T
import Text.Parsec.Text (Parser(..))
import Text.Parsec ((<|>), string, many, many1, spaces)
import qualified Text.Parsec

text :: String -> Parser T.Text
text s = T.pack <$> string s

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
      <|> (string "ى" *> pure Ye)
      <|> (string "ي" *> pure Ye) -- only correct in middle form

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
  | NonJoiner -- ^ zero-width non-joiner \8204
  deriving (Show, Eq, Enum)

instance Parse [Mod] where
  parse = (string "\1611" *> pure [Fethateyn])
      <|> (string "\1612" *> pure [Kesrateyn])
      <|> (string "\1613" *> pure [Dammeteyn])
      <|> (string "\1614" *> pure [Üstün])
      <|> (string "\1615" *> pure [Ötre])
      <|> (string "\1616" *> pure [Esre])
      <|> (string "\1617" *> pure [Şedde])
      <|> (string "\1618" *> pure [Sükun])
      <|> (string "\1619" *> pure [Medde]) -- normal size
      <|> (string "\1620" *> pure [Hemze, Üstün]) -- above
      <|> (string "\1621" *> pure [Hemze, Esre]) -- below
      <|> (string "\1764" *> pure [Medde]) -- small high
      <|> (string "\8204" *> pure [NonJoiner])

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
    -- that stand for an Ottoman letter with a modification
          (string "آ" *> pure (ModifiedOtto [Medde] Elif))
      <|> (string "أ" *> pure (ModifiedOtto [Hemze, Üstün] Elif))
      <|> (string "إ" *> pure (ModifiedOtto [Hemze, Esre] Elif))
      <|> (string "ئ" *> pure (ModifiedOtto [Hemze] Ye))
      <|> (string "ﺌ" *> pure (ModifiedOtto [Hemze] Ye))
      <|> (string "ﺋ" *> pure (ModifiedOtto [Hemze] Ye))
      <|> (do otto <- parse @Otto
              mods <- concat <$> many (parse @[Mod])
              pure (ModifiedOtto mods otto))

data OttoText =
    Word [OttoModified]
  | Punctuation T.Text
  deriving (Show, Eq)

baseEq :: [OttoModified] -> [OttoModified] -> Bool
baseEq xs ys = (map baseOtto xs) == (map baseOtto ys)

instance Parse OttoText where
  parse = (Word <$> (spaces *> many1 (parse @OttoModified) <* spaces))
      <|> (Punctuation <$> (text "." 
                        <|> text "," 
                        <|> text "!" 
                        <|> text "؟" 
                        <|> text "?" 
                        <|> text "-" 
                        <|> text "،"))

runParser :: T.Text -> Either Text.Parsec.ParseError [OttoText]
runParser = Text.Parsec.parse (many parse) ""
