{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Server where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Daylatro.Handler.Common as Common
import qualified Daylatro.Type.Context as Context
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.IO as IO

contextToSettings :: Context.Context -> Warp.Settings
contextToSettings context =
  let host = Context.host context
      port = Context.port context
   in Warp.defaultSettings
        & Warp.setBeforeMainLoop
          ( logLn $
              F.sformat
                ("Listening on " % F.shown % " port " % F.int)
                host
                port
          )
        & Warp.setHost host
        & Warp.setLogger
          ( \request status _ ->
              logLn $
                F.sformat
                  (F.stext % " " % F.stext % " " % F.int)
                  (Encoding.decodeUtf8Lenient $ Wai.requestMethod request)
                  (Encoding.decodeUtf8Lenient $ Wai.rawPathInfo request)
                  (Http.statusCode status)
          )
        & Warp.setOnException (const $ IO.hPutStrLn IO.stderr . Exception.displayException)
        & Warp.setOnExceptionResponse (const $ Common.statusResponse Http.internalServerError500 [])
        & Warp.setPort port
        & Warp.setServerName ByteString.empty

logLn :: Text.Text -> IO ()
logLn message = do
  now <- Time.getCurrentTime
  F.fprintLn (F.customTimeFmt "%Y-%m-%dT%H:%M:%S%3QZ " % F.stext) now message
