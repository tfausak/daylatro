module Daylatro.Exception.InvalidOption where

import qualified Control.Monad.Catch as Exception

newtype InvalidOption
  = MkInvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption
