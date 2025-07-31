module Daylatro.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Application as Application
import qualified Daylatro.Constant.Migration as Migration
import qualified Daylatro.Exception.InvalidOption as InvalidOption
import qualified Daylatro.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Daylatro.Exception.UnknownOption as UnknownOption
import qualified Daylatro.Server as Server
import qualified Daylatro.Type.Config as Config
import qualified Daylatro.Type.Context as Context
import qualified Daylatro.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified PackageInfo_daylatro as Package
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  arguments <- Environment.getArgs
  mainWith arguments

mainWith :: [String] -> IO ()
mainWith arguments = do
  IO.hSetBuffering IO.stdout IO.LineBuffering

  let (flags, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute Flag.options arguments
  Monad.forM_ errs $ Exception.throwM . InvalidOption.MkInvalidOption . Text.pack
  Monad.forM_ opts $ Exception.throwM . UnknownOption.MkUnknownOption . Text.pack
  Monad.forM_ args $ Exception.throwM . UnexpectedArgument.MkUnexpectedArgument . Text.pack

  config <- Monad.foldM Config.applyFlag Config.initial flags
  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name Flag.options
    Exit.exitSuccess

  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion Package.version
    Exit.exitSuccess

  Sql.withConnection (Config.database config) $ \connection -> do
    let context = Context.fromConfig config connection
    mapM_ (Sql.execute_ $ Context.connection context) Migration.all
    Warp.runSettings (Server.contextToSettings context) $ Application.application context
