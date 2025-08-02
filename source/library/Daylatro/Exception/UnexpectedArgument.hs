module Daylatro.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text

newtype UnexpectedArgument
  = MkUnexpectedArgument Text.Text
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument
