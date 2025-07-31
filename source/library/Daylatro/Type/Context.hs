{-# LANGUAGE NamedFieldPuns #-}

module Daylatro.Type.Context where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Type.Config as Config
import qualified Network.Wai.Handler.Warp as Warp

data Context = MkContext
  { baseUrl :: Text.Text,
    connection :: Sql.Connection,
    host :: Warp.HostPreference,
    port :: Warp.Port
  }

fromConfig :: Config.Config -> Sql.Connection -> Context
fromConfig config connection =
  MkContext
    { baseUrl = Config.baseUrl config,
      connection,
      host = Config.host config,
      port = Config.port config
    }
