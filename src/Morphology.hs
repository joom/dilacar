{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Morphology where

import Ottoman

data Suffix =
    Ler -- ler, lar
  | In -- in, ın, un, ün
  | Di -- di, dı, du, dü, ti, tı, tu, tü
  | Miş -- miş, mış, muş, müş
  | Ecek -- ecek, acak
  deriving (Show, Eq, Enum)

addSuffix :: String -> Suffix -> String
addSuffix w sf = undefined

suffixOttoman :: Suffix -> [[Otto]]
suffixOttoman = \case
  Ler -> [[Lam, Re]]
  In -> [[Kef], [Nef]]
  Di -> [[Dal, Ye]]
  Miş -> [[Mim, Şın]]
  Ecek -> [[Cim, Kef], [Cim, Kaf]]
