{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Handler.Common where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Daylatro.Constant.Favicon as Favicon
import Formatting ((%))
import qualified Formatting as F
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai

htmlResponse :: Http.Status -> Http.ResponseHeaders -> H.Html a -> Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, "text/html;charset=utf-8") : headers)
    . H.renderBS

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  htmlResponse status headers . template mempty $ do
    H.h2_ $ do
      "HTTP "
      H.toHtml . show $ Http.statusCode status
    H.p_ . H.toHtml $ Http.statusMessage status

template :: H.Html () -> H.Html () -> H.Html ()
template header content = do
  H.doctype_
  H.html_ [H.lang_ "en-US"] $ do
    H.head_ $ do
      H.meta_ [H.charset_ "utf-8"]
      H.meta_ [H.name_ "viewport", H.content_ "initial-scale = 1, width = device-width"]
      H.link_
        [ H.href_ $ F.sformat ("data:image/svg+xml," % F.string) (Uri.escapeURIString Uri.isUnescapedInURIComponent Favicon.daylatro),
          H.rel_ "icon",
          H.type_ "image/svg+xml"
        ]
      H.link_
        [ H.href_ "/feed.atom",
          H.rel_ "alternate",
          H.type_ "application/atom+xml"
        ]
      H.title_ "Daylatro"
      header
    H.body_ $ do
      H.header_
        . H.h1_
        $ H.a_ [H.href_ "/"] "Daylatro"
      H.main_ content
      H.footer_
        . H.p_
        $ H.a_ [H.href_ "https://github.com/tfausak/daylatro"] "tfausak/daylatro"

parseDay :: Text.Text -> Maybe Time.Day
parseDay = Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" . Text.unpack
