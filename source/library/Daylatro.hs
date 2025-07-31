{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Daylatro where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Default as Default
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Constant.Favicon as Favicon
import qualified Daylatro.Constant.Script as Script
import qualified Daylatro.Constant.Shader as Shader
import qualified Daylatro.Constant.Style as Style
import qualified Daylatro.Exception.InvalidOption as InvalidOption
import qualified Daylatro.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Daylatro.Exception.UnknownOption as UnknownOption
import qualified Daylatro.Type.Config as Config
import qualified Daylatro.Type.Flag as Flag
import qualified Daylatro.Type.Model as Model
import qualified Daylatro.Type.Score as Score
import qualified Daylatro.Type.Seed as Seed
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified PackageInfo_daylatro as Package
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read
import qualified Text.XML as X

main :: IO ()
main = do
  arguments <- Environment.getArgs
  mainWith arguments

mainWith :: [String] -> IO ()
mainWith arguments = do
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
    IO.hSetBuffering IO.stdout IO.LineBuffering
    mapM_ (Sql.execute_ connection) migrations
    Warp.runSettings (settings config) $ application connection config

migrations :: [Sql.Query]
migrations =
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

application :: Sql.Connection -> Config.Config -> Wai.Application
application connection config request respond = case Wai.pathInfo request of
  [] -> case Http.parseMethod $ Wai.requestMethod request of
    Right Http.GET -> getIndex connection request respond
    Right Http.POST -> postIndex connection request respond
    _ -> respond $ statusResponse Http.methodNotAllowed405 []
  ["feed.atom"] -> case Http.parseMethod $ Wai.requestMethod request of
    Right Http.GET -> getFeed config respond
    _ -> respond $ statusResponse Http.methodNotAllowed405 []
  _ -> respond $ statusResponse Http.notFound404 []

getIndex ::
  Sql.Connection ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
getIndex connection request respond = do
  today <- fmap Time.utctDay Time.getCurrentTime
  let day = case lookupDay request of
        Just d
          | d < epoch -> epoch
          | d > today -> today
          | otherwise -> d
        Nothing -> today
      maybePrevious =
        if day == epoch
          then Nothing
          else Just $ Time.addDays (-1) day
      maybeNext =
        if day == today
          then Nothing
          else Just $ Time.addDays 1 day
  scores <-
    Sql.query
      connection
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
          H.toHtml $ formatDay day
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
              H.value_ $ formatDay day
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
    . htmlResponse Http.ok200 []
    $ template header content

postIndex ::
  Sql.Connection ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
postIndex connection request respond = do
  now <- Time.getCurrentTime
  query <- Http.parseQueryText . LazyByteString.toStrict <$> Wai.consumeRequestBodyLazy request
  let maybeScore = do
        day <-
          parseDay
            . Maybe.fromMaybe ""
            . Monad.join
            $ lookup "day" query
        name <- fmap (Text.map Char.toUpper) . Monad.join $ lookup "name" query
        Monad.guard . between 1 3 $ Text.length name
        Monad.guard $ Text.all (\c -> between 'A' 'Z' c || between '0' '9' c) name
        ante <-
          Read.readMaybe
            . maybe "" Text.unpack
            . Monad.join
            $ lookup "ante" query
        Monad.guard $ between 0 39 ante
        bestHand <- case lookup "bestHand" query of
          Nothing -> pure Nothing
          Just Nothing -> pure Nothing
          Just (Just text)
            | Text.null text -> pure Nothing
            | otherwise -> case Read.readMaybe $ Text.unpack text of
                Nothing -> Nothing
                Just double -> pure $ Just double
        Monad.guard $ maybe True (>= 0) bestHand
        Monad.guard $ maybe True (not . isInfinite) bestHand
        pure
          Score.MkScore
            { Score.createdAt = now,
              Score.day = day,
              Score.name = name,
              Score.ante = ante,
              Score.bestHand = bestHand
            }
  case maybeScore of
    Nothing -> respond $ statusResponse Http.badRequest400 []
    Just score -> do
      Sql.execute
        connection
        "insert into Score (createdAt, day, name, ante, bestHand) \
        \ values (?, ?, ?, ?, ?)"
        score
      respond $
        statusResponse
          Http.found302
          [(Http.hLocation, F.formatted Encoding.encodeUtf8 ("/?day=" % F.dateDash) (Score.day score))]

getFeed ::
  Config.Config ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
getFeed config respond = do
  today <- fmap Time.utctDay Time.getCurrentTime
  respond
    . Wai.responseLBS Http.ok200 [(Http.hContentType, "application/atom+xml;charset=utf-8")]
    $ X.renderLBS
      Default.def
      X.Document
        { X.documentPrologue =
            X.Prologue
              { X.prologueBefore = [],
                X.prologueDoctype = Nothing,
                X.prologueAfter = []
              },
          X.documentRoot =
            X.Element
              { X.elementName = "feed",
                X.elementAttributes = Map.singleton "xmlns" "http://www.w3.org/2005/Atom",
                X.elementNodes =
                  X.NodeElement
                    X.Element
                      { X.elementName = "id",
                        X.elementAttributes = Map.empty,
                        X.elementNodes = [X.NodeContent $ F.sformat (F.stext % "/feed.atom") (Config.baseUrl config)]
                      }
                    : X.NodeElement
                      X.Element
                        { X.elementName = "link",
                          X.elementAttributes =
                            Map.fromList
                              [ ("rel", "self"),
                                ("href", F.sformat (F.stext % "/feed.atom") (Config.baseUrl config))
                              ],
                          X.elementNodes = []
                        }
                    : X.NodeElement
                      X.Element
                        { X.elementName = "title",
                          X.elementAttributes = Map.empty,
                          X.elementNodes = [X.NodeContent "Daylatro"]
                        }
                    : X.NodeElement
                      X.Element
                        { X.elementName = "updated",
                          X.elementAttributes = Map.empty,
                          X.elementNodes = [X.NodeContent . Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT00:00:00Z" today]
                        }
                    : X.NodeElement
                      X.Element
                        { X.elementName = "author",
                          X.elementAttributes = Map.empty,
                          X.elementNodes =
                            [ X.NodeElement
                                X.Element
                                  { X.elementName = "name",
                                    X.elementAttributes = Map.empty,
                                    X.elementNodes = [X.NodeContent "Taylor Fausak"]
                                  }
                            ]
                        }
                    : fmap
                      ( \day ->
                          let seed = Seed.fromDay day
                              url = F.sformat (F.stext % "/?day=" % F.dateDash) (Config.baseUrl config) day
                           in X.NodeElement
                                X.Element
                                  { X.elementName = "entry",
                                    X.elementAttributes = Map.empty,
                                    X.elementNodes =
                                      [ X.NodeElement
                                          X.Element
                                            { X.elementName = "id",
                                              X.elementAttributes = Map.empty,
                                              X.elementNodes = [X.NodeContent url]
                                            },
                                        X.NodeElement
                                          X.Element
                                            { X.elementName = "link",
                                              X.elementAttributes =
                                                Map.fromList
                                                  [ ("rel", "self"),
                                                    ("href", url)
                                                  ],
                                              X.elementNodes = []
                                            },
                                        X.NodeElement
                                          X.Element
                                            { X.elementName = "title",
                                              X.elementAttributes = Map.empty,
                                              X.elementNodes = [X.NodeContent $ F.sformat ("Daily seed for " % F.dateDash) day]
                                            },
                                        X.NodeElement
                                          X.Element
                                            { X.elementName = "updated",
                                              X.elementAttributes = Map.empty,
                                              X.elementNodes = [X.NodeContent $ F.sformat (F.dateDash % "T00:00:00Z") day]
                                            },
                                        X.NodeElement
                                          X.Element
                                            { X.elementName = "content",
                                              X.elementAttributes = Map.empty,
                                              X.elementNodes = [X.NodeContent $ Seed.toText seed]
                                            }
                                      ]
                                  }
                      )
                      [epoch .. today]
              },
          X.documentEpilogue = []
        }

between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi

settings :: Config.Config -> Warp.Settings
settings config =
  let host = Config.host config
      port = Config.port config
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
        & Warp.setOnExceptionResponse (const $ statusResponse Http.internalServerError500 [])
        & Warp.setPort port
        & Warp.setServerName ByteString.empty

logLn :: Text.Text -> IO ()
logLn message = do
  now <- Time.getCurrentTime
  F.fprintLn (F.customTimeFmt "%Y-%m-%dT%H:%M:%S%3QZ " % F.stext) now message

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

lookupDay :: Wai.Request -> Maybe Time.Day
lookupDay request = do
  maybeByteString <- lookup "day" $ Wai.queryString request
  byteString <- maybeByteString
  text <- either (const Nothing) Just $ Encoding.decodeUtf8' byteString
  parseDay text

parseDay :: Text.Text -> Maybe Time.Day
parseDay = Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" . Text.unpack

formatDay :: Time.Day -> Text.Text
formatDay = Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

epoch :: Time.Day
epoch = Time.fromGregorian 2024 2 20
