module Daylatro.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception

newtype UnknownOption
  = MkUnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption
