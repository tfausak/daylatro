{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Application where

import qualified Daylatro.Handler.Common as Common
import qualified Daylatro.Handler.Feed.Get as GetFeed
import qualified Daylatro.Handler.Index.Get as GetIndex
import qualified Daylatro.Handler.Index.Post as PostIndex
import qualified Daylatro.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Context.Context -> Wai.Application
application context request respond = case Wai.pathInfo request of
  [] -> case Http.parseMethod $ Wai.requestMethod request of
    Right Http.GET -> GetIndex.handler context request respond
    Right Http.POST -> PostIndex.handler context request respond
    _ -> respond $ Common.statusResponse Http.methodNotAllowed405 []
  ["feed.atom"] -> case Http.parseMethod $ Wai.requestMethod request of
    Right Http.GET -> GetFeed.handler context respond
    _ -> respond $ Common.statusResponse Http.methodNotAllowed405 []
  _ -> respond $ Common.statusResponse Http.notFound404 []
