{-# LANGUAGE MultilineStrings #-}

module Daylatro.Constant.Migration where

import qualified Data.String as String
import qualified Database.SQLite.Simple as Sql

all :: [Sql.Query]
all =
  fmap
    String.fromString
    [ """
      create table if not exists Score
        ( key integer primary key
        , createdAt text not null
        , day text not null
        , name text not null
        , ante integer not null
        , bestHand real )
      """
    ]
