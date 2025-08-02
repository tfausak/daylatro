module Daylatro.Type.Key where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql

newtype Key a = MkKey
  { value :: Int
  }
  deriving (Eq, Show)

instance Sql.FromField (Key a) where
  fromField = fmap MkKey . Sql.fromField

instance Sql.ToField (Key a) where
  toField = Sql.toField . value
