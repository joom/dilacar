{-# LANGUAGE LambdaCase #-}
module Lib where

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

renderOtto :: Otto -> String
renderOtto = \case
  Elif -> "ا"
  Be   -> "ب"
  Pe   -> "پ"
  Te   -> "ت"
  Se   -> "ث"
  Cim  -> "ج"
  Çim  -> "چ"
  Ha   -> "ح"
  Hı   -> "خ"
  Dal  -> "د"
  Zel  -> "ذ"
  Re   -> "ر"
  Ze   -> "ز"
  Je   -> "ژ"
  Sin  -> "س"
  Şın  -> "ش"
  Sad  -> "ص"
  Dad  -> "ض"
  Tı   -> "ط"
  Zı   -> "ظ"
  Ayn  -> "ع"
  Gayn -> "غ"
  Fe   -> "ف"
  Kaf  -> "ق"
  Kef  -> "ك"
  Gef  -> "گ"
  Nef  -> "ڭ"
  Lam  -> "ل"
  Mim  -> "م"
  Nun  -> "ن"
  Vav  -> "و"
  He   -> "ه"
  Ye   -> "ی"

parseOtto :: String -> Otto
parseOtto = \case
  "ا" -> Elif
  "ب" -> Be
  "پ" -> Pe
  "ت" -> Te
  "ث" -> Se
  "ج" -> Cim
  "چ" -> Çim
  "ح" -> Ha
  "خ" -> Hı
  "د" -> Dal
  "ذ" -> Zel
  "ر" -> Re
  "ز" -> Ze
  "ژ" -> Je
  "س" -> Sin
  "ش" -> Şın
  "ص" -> Sad
  "ض" -> Dad
  "ط" -> Tı
  "ظ" -> Zı
  "ع" -> Ayn
  "غ" -> Gayn
  "ف" -> Fe
  "ق" -> Kaf
  "ك" -> Kef
  "گ" -> Gef
  "ڭ" -> Nef
  "ل" -> Lam
  "م" -> Mim
  "ن" -> Nun
  "و" -> Vav
  "ه" -> He
  "ی" -> Ye

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
