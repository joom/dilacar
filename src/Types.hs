module Types where

import Control.Monad.Trans.State.Strict

import qualified Ottoman as Otto

data DilError =
    NoVowel String

data DilState = DilState
  { helperDict :: [([Otto.OttoModified], String)]
  , errors :: [DilError]
  }

type DM a = StateT DilState IO a
