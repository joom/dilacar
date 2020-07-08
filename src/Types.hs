module Types where

import Control.Monad.Trans.State.Strict

import qualified Ottoman as Otto
import qualified Data.Text as T

data DilError =
    NoVowel T.Text

data DilState = DilState
  { helperDict :: [([Otto.OttoModified], T.Text)]
  , errors     :: [DilError]
  }

-- | Dilacar monad transformer
type DM a = StateT DilState IO a
