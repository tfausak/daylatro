{-# LANGUAGE NamedFieldPuns #-}

module Daylatro.Type.Model where

import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Type.Key as Key

data Model a = MkModel
  { key :: Key.Key a,
    value :: a
  }
  deriving (Eq, Show)

instance (Sql.FromRow a) => Sql.FromRow (Model a) where
  fromRow = do
    key <- Sql.field
    value <- Sql.fromRow
    pure MkModel {key, value}

instance (Sql.ToRow a) => Sql.ToRow (Model a) where
  toRow model = Sql.toRow $ Sql.Only (key model) Sql.:. value model
