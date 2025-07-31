module Daylatro.Exception.InvalidOption where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text

newtype InvalidOption
  = MkInvalidOption Text.Text
  deriving (Eq, Show)

instance Exception.Exception InvalidOption
