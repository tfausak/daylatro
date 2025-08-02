{-# LANGUAGE NamedFieldPuns #-}

module Daylatro.Type.Score where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql

data Score = MkScore
  { createdAt :: Time.UTCTime,
    day :: Time.Day,
    name :: Text.Text,
    ante :: Int,
    bestHand :: Maybe Double
  }
  deriving (Eq, Show)

instance Sql.FromRow Score where
  fromRow = do
    createdAt <- Sql.field
    day <- Sql.field
    name <- Sql.field
    ante <- Sql.field
    bestHand <- Sql.field
    pure
      MkScore
        { createdAt,
          day,
          name,
          ante,
          bestHand
        }

instance Sql.ToRow Score where
  toRow score =
    Sql.toRow
      ( createdAt score,
        day score,
        name score,
        ante score,
        bestHand score
      )
