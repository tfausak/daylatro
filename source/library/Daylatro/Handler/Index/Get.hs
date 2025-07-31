{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Handler.Index.Get where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Constant.Balatro as Balatro
import qualified Daylatro.Constant.Script as Script
import qualified Daylatro.Constant.Shader as Shader
import qualified Daylatro.Constant.Style as Style
import qualified Daylatro.Handler.Common as Common
import qualified Daylatro.Type.Context as Context
import qualified Daylatro.Type.Model as Model
import qualified Daylatro.Type.Score as Score
import qualified Daylatro.Type.Seed as Seed
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler ::
  Context.Context ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
handler context request respond = do
  today <- fmap Time.utctDay Time.getCurrentTime
  let day = case lookupDay request of
        Just d
          | d < Balatro.epoch -> Balatro.epoch
          | d > today -> today
          | otherwise -> d
        Nothing -> today
      maybePrevious =
        if day == Balatro.epoch
          then Nothing
          else Just $ Time.addDays (-1) day
      maybeNext =
        if day == today
          then Nothing
          else Just $ Time.addDays 1 day
  scores <-
    Sql.query
      (Context.connection context)
      """
      select *
      from Score
      where day = ?
      order by ante desc, bestHand desc
      limit 10
      """
      [day]
  let seed = Seed.fromDay day
      header :: H.Html ()
      header = do
        H.meta_
          [ H.term "property" "og:type",
            H.content_ "website"
          ]
        H.meta_
          [ H.term "property" "og:title",
            H.content_ "Daylatro"
          ]
        H.meta_
          [ H.term "property" "og:description",
            H.content_ $ F.sformat ("The Balatro daily seed for " % F.dateDash % " is " % Seed.format % ".") day seed
          ]
        H.script_ Script.shaderWebBackground
        H.script_
          [H.id_ "balatroShader", H.type_ "x-shader/x-fragment"]
          Shader.balatro
        H.script_ Script.daylatro
        H.style_ Style.daylatro
      content :: H.Html ()
      content = do
        H.p_ $ do
          "The "
          H.a_ [H.href_ "https://www.playbalatro.com"] "Balatro"
          " daily seed for "
          H.br_ []
          case maybePrevious of
            Nothing -> "<-"
            Just previous ->
              H.a_
                [ H.href_ $ F.sformat ("/?day=" % F.dateDash) previous,
                  H.title_ "Go to previous day."
                ]
                "<-"
          " "
          H.toHtml $ F.format F.dateDash day
          " "
          case maybeNext of
            Nothing -> "->"
            Just next ->
              H.a_
                [ H.href_ $ F.sformat ("/?day=" % F.dateDash) next,
                  H.title_ "Go to next day."
                ]
                "->"
          H.br_ []
          " is "
          H.span_
            [ H.onclick_ $ F.sformat ("navigator.clipboard.writeText('" % Seed.format % "');") seed,
              H.title_ "Click to copy."
            ]
            $ H.toHtml seed
          "."
        H.form_ [H.method_ "post"] $ do
          H.input_
            [ H.name_ "day",
              H.type_ "hidden",
              H.value_ $ F.sformat F.dateDash day
            ]
          H.table_ $ do
            H.thead_ . H.tr_ $ do
              H.th_ "Name"
              H.th_ "Ante"
              H.th_ "Best Hand"
            H.tbody_ $ do
              Monad.forM_ scores $ \score -> H.tr_ $ do
                H.td_ . H.toHtml . Score.name $ Model.value score
                H.td_ . H.toHtml . show . Score.ante $ Model.value score
                H.td_
                  . H.toHtml
                  . (\x -> Maybe.fromMaybe x $ Text.stripSuffix ".0" x)
                  . Text.pack
                  . maybe "" show
                  . Score.bestHand
                  $ Model.value score
              H.tr_ $ do
                H.td_ $
                  H.input_
                    [ H.maxlength_ "3",
                      H.minlength_ "1",
                      H.name_ "name",
                      H.pattern_ "[A-Za-z0-9]+",
                      H.placeholder_ "ABC",
                      H.required_ "required",
                      H.size_ "4"
                    ]
                H.td_ $
                  H.input_
                    [ H.max_ "39",
                      H.min_ "0",
                      H.name_ "ante",
                      H.placeholder_ "8",
                      H.required_ "required",
                      H.size_ "3",
                      H.type_ "number"
                    ]
                H.td_ $
                  H.input_
                    [ H.min_ "0",
                      H.name_ "bestHand",
                      H.placeholder_ "123456",
                      H.size_ "10",
                      H.type_ "number"
                    ]
          H.button_ [H.type_ "submit"] "Submit"
  respond
    . Common.htmlResponse Http.ok200 []
    $ Common.template header content

lookupDay :: Wai.Request -> Maybe Time.Day
lookupDay request = do
  maybeByteString <- lookup "day" $ Wai.queryString request
  byteString <- maybeByteString
  text <- either (const Nothing) Just $ Encoding.decodeUtf8' byteString
  Common.parseDay text
