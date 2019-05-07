{-# LANGUAGE LambdaCase #-}
module Lib where

class Parse a where
  parse :: String -> Maybe a

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
  parse = \case
    "ا" -> Just Elif
    "ب" -> Just Be
    "پ" -> Just Pe
    "ت" -> Just Te
    "ث" -> Just Se
    "ج" -> Just Cim
    "چ" -> Just Çim
    "ح" -> Just Ha
    "خ" -> Just Hı
    "د" -> Just Dal
    "ذ" -> Just Zel
    "ر" -> Just Re
    "ز" -> Just Ze
    "ژ" -> Just Je
    "س" -> Just Sin
    "ش" -> Just Şın
    "ص" -> Just Sad
    "ض" -> Just Dad
    "ط" -> Just Tı
    "ظ" -> Just Zı
    "ع" -> Just Ayn
    "غ" -> Just Gayn
    "ف" -> Just Fe
    "ق" -> Just Kaf
    "ك" -> Just Kef
    "گ" -> Just Gef
    "ڭ" -> Just Nef
    "ل" -> Just Lam
    "م" -> Just Mim
    "ن" -> Just Nun
    "و" -> Just Vav
    "ه" -> Just He
    "ی" -> Just Ye
    _ -> Nothing

data Mod =
    Hemze
  | Üstün
  | Esre
  | Ötre
  | Şedde
  | Sükun
  | Medde
  | TenvinÜstün -- ^ Also known as "fethateyn" (iki üstün)
  | TenvinEsre  -- ^ Also known as "kesrateyn" (iki esre)
  | TenvinÖtre  -- ^ Also known as "dammeteyn" (iki ötre)

instance Parse Mod where
  parse = \case
    "\1611" -> Just TenvinÜstün
    "\1612" -> Just TenvinÖtre
    "\1613" -> Just TenvinEsre
    "\1614" -> Just Üstün
    "\1615" -> Just Ötre
    "\1616" -> Just Esre
    "\1617" -> Just Şedde
    "\1618" -> Just Sükun
    "\1764" -> Just Medde
    _ -> Nothing

data OttoModified =
    PureOtto Otto
  | ModifiedOtto [Mod] Otto

instance Parse OttoModified where
  parse = \case
    -- Exception cases for the special Unicode characters
    -- that stand for an Ottoman leter with a modification
    "\1570" -> Just $ ModifiedOtto [Medde] Elif
    "\1571" -> Just $ ModifiedOtto [Hemze] Elif -- Hemze on top
    "\1573" -> Just $ ModifiedOtto [Hemze] Elif -- Hemze at bottom
    _ -> Nothing

data Suffix =
    Ler -- ler, lar
  | In -- in, ın, un, ün
  | Di -- di, dı, du, dü, ti, tı, tu, tü
  | Miş -- miş, mış, muş, müş
  | Ecek -- ecek, acak
  deriving (Show, Eq, Enum)

suffixOttoman :: Suffix -> [[Otto]]
suffixOttoman = \case
  Ler -> [[Lam, Re]]
  In -> [[Kef], [Nef]]
  Di -> [[Dal, Ye]]
  Miş -> [[Mim, Şın]]
  Ecek -> [[Cim, Kef], [Cim, Kaf]]

type Result = [String]

word :: String -> Result
word w = undefined

entry :: String -> [Result]
entry s = map word (words s)
