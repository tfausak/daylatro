module Daylatro.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Exception

newtype UnexpectedArgument
  = MkUnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument
