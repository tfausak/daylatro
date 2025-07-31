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
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Daylatro.Constant.Script as Script
import qualified Daylatro.Constant.Shader as Shader
import qualified Daylatro.Constant.Style as Style
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Lucid as Html
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified PackageInfo_daylatro as Package
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Random as Random
import qualified Text.Read as Read
import qualified Text.XML as Xml

main :: IO ()
main = do
  arguments <- Environment.getArgs
  mainWith arguments

mainWith :: [String] -> IO ()
mainWith arguments = do
  flags <- do
    let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
    Monad.forM_ errs $ Exception.throwM . MkInvalidOption
    Monad.forM_ opts $ Exception.throwM . MkUnkownOption
    Monad.forM_ args $ Exception.throwM . MkUnexpectedArgument
    pure flgs

  config <- Monad.foldM applyFlag initialConfig flags
  Monad.when (configHelp config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name options
    Exit.exitSuccess

  Monad.when (configVersion config) $ do
    putStrLn $ Version.showVersion Package.version
    Exit.exitSuccess

  Sql.withConnection (configDatabase config) $ \connection -> do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Sql.execute_
      connection
      "create table if not exists Score \
      \ ( key integer primary key \
      \ , createdAt text not null \
      \ , day text not null \
      \ , name text not null \
      \ , ante integer not null \
      \ , bestHand real )"
    Warp.runSettings (settings config) $ application connection config

newtype InvalidOption
  = MkInvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption

newtype UnkownOption
  = MkUnkownOption String
  deriving (Eq, Show)

instance Exception.Exception UnkownOption

newtype UnexpectedArgument
  = MkUnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument

options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option ['h'] ["help"] (GetOpt.NoArg FlagHelp) "",
    GetOpt.Option [] ["version"] (GetOpt.NoArg FlagVersion) "",
    GetOpt.Option [] ["base-url"] (GetOpt.ReqArg FlagBaseUrl "URL") "",
    GetOpt.Option [] ["database"] (GetOpt.ReqArg FlagDatabase "STRING") "",
    GetOpt.Option [] ["host"] (GetOpt.ReqArg FlagHost "STRING") "",
    GetOpt.Option [] ["port"] (GetOpt.ReqArg FlagPort "INT") ""
  ]

data Flag
  = FlagBaseUrl String
  | FlagDatabase String
  | FlagHelp
  | FlagHost String
  | FlagPort String
  | FlagVersion
  deriving (Eq, Show)

data Config = MkConfig
  { configBaseUrl :: String,
    configDatabase :: FilePath,
    configHelp :: Bool,
    configHost :: Warp.HostPreference,
    configPort :: Warp.Port,
    configVersion :: Bool
  }
  deriving (Eq, Show)

initialConfig :: Config
initialConfig =
  MkConfig
    { configBaseUrl = "",
      configDatabase = ":memory:",
      configHelp = False,
      configHost = "127.0.0.1",
      configPort = 8080,
      configVersion = False
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag -> m Config
applyFlag config flag = case flag of
  FlagBaseUrl string -> pure config {configBaseUrl = string}
  FlagDatabase database -> pure config {configDatabase = database}
  FlagHelp -> pure config {configHelp = True}
  FlagHost string -> pure config {configHost = String.fromString string}
  FlagPort string -> case Read.readMaybe string of
    Nothing -> Exception.throwM . MkInvalidOption $ "invalid port: " <> show string
    Just port -> pure config {configPort = port}
  FlagVersion -> pure config {configVersion = True}

application :: Sql.Connection -> Config -> Wai.Application
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
      "select * \
      \ from Score \
      \ where day = ? \
      \ order by ante desc, bestHand desc \
      \ limit 10"
      [day]
  let seed = getSeed day
      header :: Html.Html ()
      header = do
        Html.meta_
          [ Html.term "property" "og:type",
            Html.content_ "website"
          ]
        Html.meta_
          [ Html.term "property" "og:title",
            Html.content_ "Daylatro"
          ]
        Html.meta_
          [ Html.term "property" "og:description",
            Html.content_ $ F.sformat ("The Balatro daily seed for " % F.dateDash % " is " % formatSeed % ".") day seed
          ]
        Html.script_ Script.shaderWebBackground
        Html.script_
          [Html.id_ "balatroShader", Html.type_ "x-shader/x-fragment"]
          Shader.balatro
        Html.script_ Script.daylatro
        Html.style_ Style.daylatro
      content :: Html.Html ()
      content = do
        Html.p_ $ do
          "The "
          Html.a_ [Html.href_ "https://www.playbalatro.com"] "Balatro"
          " daily seed for "
          Html.br_ []
          case maybePrevious of
            Nothing -> "<-"
            Just previous ->
              Html.a_
                [ Html.href_ . Text.pack $ "/?day=" <> formatDay previous,
                  Html.title_ "Go to previous day."
                ]
                "<-"
          " "
          Html.toHtml $ formatDay day
          " "
          case maybeNext of
            Nothing -> "->"
            Just next ->
              Html.a_
                [ Html.href_ . Text.pack $ "/?day=" <> formatDay next,
                  Html.title_ "Go to next day."
                ]
                "->"
          Html.br_ []
          " is "
          Html.span_
            [ Html.onclick_ $ "navigator.clipboard.writeText('" <> seedToText seed <> "');",
              Html.title_ "Click to copy."
            ]
            $ Html.toHtml seed
          "."
        Html.form_ [Html.method_ "post"] $ do
          Html.input_
            [ Html.name_ "day",
              Html.type_ "hidden",
              Html.value_ . Text.pack $ formatDay day
            ]
          Html.table_ $ do
            Html.thead_ . Html.tr_ $ do
              Html.th_ "Name"
              Html.th_ "Ante"
              Html.th_ "Best Hand"
            Html.tbody_ $ do
              Monad.forM_ scores $ \score -> Html.tr_ $ do
                Html.td_ . Html.toHtml . scoreName $ modelValue score
                Html.td_ . Html.toHtml . show . scoreAnte $ modelValue score
                Html.td_
                  . Html.toHtml
                  . (\x -> Maybe.fromMaybe x $ Text.stripSuffix ".0" x)
                  . Text.pack
                  . maybe "" show
                  . scoreBestHand
                  $ modelValue score
              Html.tr_ $ do
                Html.td_ $
                  Html.input_
                    [ Html.maxlength_ "3",
                      Html.minlength_ "1",
                      Html.name_ "name",
                      Html.pattern_ "[A-Za-z0-9]+",
                      Html.placeholder_ "ABC",
                      Html.required_ "required",
                      Html.size_ "4"
                    ]
                Html.td_ $
                  Html.input_
                    [ Html.max_ "39",
                      Html.min_ "0",
                      Html.name_ "ante",
                      Html.placeholder_ "8",
                      Html.required_ "required",
                      Html.size_ "3",
                      Html.type_ "number"
                    ]
                Html.td_ $
                  Html.input_
                    [ Html.min_ "0",
                      Html.name_ "bestHand",
                      Html.placeholder_ "123456",
                      Html.size_ "10",
                      Html.type_ "number"
                    ]
          Html.button_ [Html.type_ "submit"] "Submit"
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
            . maybe "" Text.unpack
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
          MkScore
            { scoreCreatedAt = now,
              scoreDay = day,
              scoreName = name,
              scoreAnte = ante,
              scoreBestHand = bestHand
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
          [(Http.hLocation, mappend "/?day=" . Encoding.encodeUtf8 . Text.pack . formatDay $ scoreDay score)]

getFeed ::
  Config ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
getFeed config respond = do
  today <- fmap Time.utctDay Time.getCurrentTime
  respond
    . Wai.responseLBS Http.ok200 [(Http.hContentType, "application/atom+xml;charset=utf-8")]
    $ Xml.renderLBS
      Default.def
      Xml.Document
        { Xml.documentPrologue =
            Xml.Prologue
              { Xml.prologueBefore = [],
                Xml.prologueDoctype = Nothing,
                Xml.prologueAfter = []
              },
          Xml.documentRoot =
            Xml.Element
              { Xml.elementName = "feed",
                Xml.elementAttributes = Map.singleton "xmlns" "http://www.w3.org/2005/Atom",
                Xml.elementNodes =
                  Xml.NodeElement
                    Xml.Element
                      { Xml.elementName = "id",
                        Xml.elementAttributes = Map.empty,
                        Xml.elementNodes = [Xml.NodeContent . Text.pack $ configBaseUrl config <> "/feed.atom"]
                      }
                    : Xml.NodeElement
                      Xml.Element
                        { Xml.elementName = "link",
                          Xml.elementAttributes =
                            Map.fromList
                              [ ("rel", "self"),
                                ("href", Text.pack $ configBaseUrl config <> "/feed.atom")
                              ],
                          Xml.elementNodes = []
                        }
                    : Xml.NodeElement
                      Xml.Element
                        { Xml.elementName = "title",
                          Xml.elementAttributes = Map.empty,
                          Xml.elementNodes = [Xml.NodeContent "Daylatro"]
                        }
                    : Xml.NodeElement
                      Xml.Element
                        { Xml.elementName = "updated",
                          Xml.elementAttributes = Map.empty,
                          Xml.elementNodes = [Xml.NodeContent . Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT00:00:00Z" today]
                        }
                    : Xml.NodeElement
                      Xml.Element
                        { Xml.elementName = "author",
                          Xml.elementAttributes = Map.empty,
                          Xml.elementNodes =
                            [ Xml.NodeElement
                                Xml.Element
                                  { Xml.elementName = "name",
                                    Xml.elementAttributes = Map.empty,
                                    Xml.elementNodes = [Xml.NodeContent "Taylor Fausak"]
                                  }
                            ]
                        }
                    : fmap
                      ( \day ->
                          let seed = getSeed day
                              date = Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" day
                              url = Text.pack (configBaseUrl config) <> "/?day=" <> date
                           in Xml.NodeElement
                                Xml.Element
                                  { Xml.elementName = "entry",
                                    Xml.elementAttributes = Map.empty,
                                    Xml.elementNodes =
                                      [ Xml.NodeElement
                                          Xml.Element
                                            { Xml.elementName = "id",
                                              Xml.elementAttributes = Map.empty,
                                              Xml.elementNodes = [Xml.NodeContent url]
                                            },
                                        Xml.NodeElement
                                          Xml.Element
                                            { Xml.elementName = "link",
                                              Xml.elementAttributes =
                                                Map.fromList
                                                  [ ("rel", "self"),
                                                    ("href", url)
                                                  ],
                                              Xml.elementNodes = []
                                            },
                                        Xml.NodeElement
                                          Xml.Element
                                            { Xml.elementName = "title",
                                              Xml.elementAttributes = Map.empty,
                                              Xml.elementNodes = [Xml.NodeContent $ "Daily seed for " <> date]
                                            },
                                        Xml.NodeElement
                                          Xml.Element
                                            { Xml.elementName = "updated",
                                              Xml.elementAttributes = Map.empty,
                                              Xml.elementNodes = [Xml.NodeContent $ date <> "T00:00:00Z"]
                                            },
                                        Xml.NodeElement
                                          Xml.Element
                                            { Xml.elementName = "content",
                                              Xml.elementAttributes = Map.empty,
                                              Xml.elementNodes = [Xml.NodeContent $ seedToText seed]
                                            }
                                      ]
                                  }
                      )
                      [epoch .. today]
              },
          Xml.documentEpilogue = []
        }

between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi

settings :: Config -> Warp.Settings
settings config =
  let host = configHost config
      port = configPort config
   in Warp.defaultSettings
        & Warp.setBeforeMainLoop
          ( logLn $
              F.formatToString
                ("Listening on " % F.shown % " port " % F.int)
                host
                port
          )
        & Warp.setHost host
        & Warp.setLogger
          ( \request status _ ->
              logLn $
                F.formatToString
                  (F.stext % " " % F.stext % " " % F.int)
                  (Encoding.decodeUtf8Lenient $ Wai.requestMethod request)
                  (Encoding.decodeUtf8Lenient $ Wai.rawPathInfo request)
                  (Http.statusCode status)
          )
        & Warp.setOnException (const $ IO.hPutStrLn IO.stderr . Exception.displayException)
        & Warp.setOnExceptionResponse (const $ statusResponse Http.internalServerError500 [])
        & Warp.setPort port
        & Warp.setServerName ByteString.empty

logLn :: String -> IO ()
logLn message = do
  now <- Time.getCurrentTime
  F.fprintLn (F.customTimeFmt "%Y-%m-%dT%H:%M:%S%3QZ" % " " % F.string) now message

htmlResponse :: Http.Status -> Http.ResponseHeaders -> Html.Html a -> Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, "text/html;charset=utf-8") : headers)
    . Html.renderBS

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  htmlResponse status headers . template mempty $ do
    Html.h2_ $ do
      "HTTP "
      Html.toHtml . show $ Http.statusCode status
    Html.p_ . Html.toHtml $ Http.statusMessage status

template :: Html.Html () -> Html.Html () -> Html.Html ()
template header content = do
  Html.doctype_
  Html.html_ [Html.lang_ "en-US"] $ do
    Html.head_ $ do
      Html.meta_ [Html.charset_ "utf-8"]
      Html.meta_ [Html.name_ "viewport", Html.content_ "initial-scale = 1, width = device-width"]
      Html.link_
        [ Html.href_ . Text.pack $ "data:image/svg+xml," <> Uri.escapeURIString Uri.isUnescapedInURIComponent "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 18 18'><text x='1' y='15'>&#x1f0cf;</text></svg>",
          Html.rel_ "icon",
          Html.type_ "image/svg+xml"
        ]
      Html.link_
        [ Html.href_ "/feed.atom",
          Html.rel_ "alternate",
          Html.type_ "application/atom+xml"
        ]
      Html.title_ "Daylatro"
      header
    Html.body_ $ do
      Html.header_
        . Html.h1_
        $ Html.a_ [Html.href_ "/"] "Daylatro"
      Html.main_ content
      Html.footer_
        . Html.p_
        $ Html.a_ [Html.href_ "https://github.com/tfausak/daylatro"] "tfausak/daylatro"

lookupDay :: Wai.Request -> Maybe Time.Day
lookupDay request = do
  maybeByteString <- lookup "day" $ Wai.queryString request
  byteString <- maybeByteString
  text <- either (const Nothing) Just $ Encoding.decodeUtf8' byteString
  parseDay $ Text.unpack text

parseDay :: String -> Maybe Time.Day
parseDay = Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d"

formatDay :: Time.Day -> String
formatDay = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

newtype Seed
  = MkSeed String
  deriving (Eq, Show)

instance Html.ToHtml Seed where
  toHtml = Html.toHtml . seedToString
  toHtmlRaw = Html.toHtmlRaw . seedToString

seedToString :: Seed -> String
seedToString (MkSeed x) = x

seedToText :: Seed -> Text.Text
seedToText = Text.pack . seedToString

formatSeed :: F.Format t (Seed -> t)
formatSeed = F.mapf seedToString F.string

getSeed :: Time.Day -> Seed
getSeed =
  MkSeed
    . fmap (toEnum . (\x -> x + if x < 10 then 48 else 55))
    . fst
    . Random.uniformListR 8 (0, 35)
    . Random.mkStdGen
    . fromIntegral
    . Time.toModifiedJulianDay

epoch :: Time.Day
epoch = Time.fromGregorian 2024 2 20

data Model a = MkModel
  { modelKey :: Key a,
    modelValue :: a
  }
  deriving (Eq, Show)

instance (Sql.FromRow a) => Sql.FromRow (Model a) where
  fromRow = do
    key <- Sql.field
    value <- Sql.fromRow
    pure MkModel {modelKey = key, modelValue = value}

instance (Sql.ToRow a) => Sql.ToRow (Model a) where
  toRow model = Sql.toRow $ Sql.Only (modelKey model) Sql.:. modelValue model

newtype Key a = MkKey
  { keyValue :: Int
  }
  deriving (Eq, Show)

instance Sql.FromField (Key a) where
  fromField = fmap MkKey . Sql.fromField

instance Sql.ToField (Key a) where
  toField = Sql.toField . keyValue

data Score = MkScore
  { scoreCreatedAt :: Time.UTCTime,
    scoreDay :: Time.Day,
    scoreName :: Text.Text,
    scoreAnte :: Int,
    scoreBestHand :: Maybe Double
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
        { scoreCreatedAt = createdAt,
          scoreDay = day,
          scoreName = name,
          scoreAnte = ante,
          scoreBestHand = bestHand
        }

instance Sql.ToRow Score where
  toRow score =
    Sql.toRow
      ( scoreCreatedAt score,
        scoreDay score,
        scoreName score,
        scoreAnte score,
        scoreBestHand score
      )
