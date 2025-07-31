module Daylatro.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text

newtype UnknownOption
  = MkUnknownOption Text.Text
  deriving (Eq, Show)

instance Exception.Exception UnknownOption
