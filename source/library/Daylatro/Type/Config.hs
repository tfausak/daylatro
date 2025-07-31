{-# LANGUAGE NamedFieldPuns #-}

module Daylatro.Type.Config where

import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Daylatro.Exception.InvalidOption as InvalidOption
import qualified Daylatro.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Config = MkConfig
  { baseUrl :: String,
    database :: FilePath,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Warp.Port,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  MkConfig
    { baseUrl = "",
      database = ":memory:",
      help = False,
      host = String.fromString "127.0.0.1",
      port = 8080,
      version = False
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.BaseUrl baseUrl -> pure config {baseUrl}
  Flag.Database database -> pure config {database}
  Flag.Help -> pure config {help = True}
  Flag.Host string -> pure config {host = String.fromString string}
  Flag.Port string -> case Read.readMaybe string of
    Nothing -> Exception.throwM . InvalidOption.MkInvalidOption $ "invalid port: " <> show string
    Just port -> pure config {port}
  Flag.Version -> pure config {version = True}
