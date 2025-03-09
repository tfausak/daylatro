{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

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
        Html.script_ shaderWebBackground
        Html.script_
          [Html.id_ "balatroShader", Html.type_ "x-shader/x-fragment"]
          balatroShader
        Html.script_ myScript
        Html.style_ myStyle
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

myScript :: Text.Text
myScript =
  """
  shaderWebBackground.shade({
    shaders: {
      balatroShader: {
        uniforms: {
          iResolution: (gl, loc, ctx) => gl.uniform2f(loc, ctx.width, ctx.height),
          iTime:       (gl, loc) => gl.uniform1f(loc, performance.now() / 1000),
        }
      }
    }
  });
  """

myStyle :: Text.Text
myStyle =
  Text.unlines
    [ "@font-face {",
      "font-family: 'Balatro';",
      "src: url(data:text/woff;base64," <> balatroFont <> ");",
      "}",
      """
      html {
        background: black;
        color: white;
        font: 16px/3em 'Balatro', sans-serif;
        letter-spacing: 1px;
        text-align: center;
        text-shadow: 0 4px 0 black;
      }
      body {
        margin: 0 auto;
        max-width: 40em;
        padding: 0 1.5em;
      }
      a {
        color: inherit;
        text-decoration: none;
      }
      header, main, footer {
        margin: 3em 0;
      }
      h1 {
        font-size: 2em;
        margin: 0;
      }
      main p {
        margin: 1.5em 0;
      }
      main a {
        color: rgb(0, 147, 255);
      }
      span {
        cursor: pointer;
      }
      table {
        text-align: left;
        width: 100%;
      }
      input {
        display: block;
        font: inherit;
      }
      button {
        background: rgb(0, 147, 255);
        border-radius: 0.5em;
        border: none;
        box-shadow: 0 4px 0 black;
        color: inherit;
        cursor: pointer;
        display: block;
        font: inherit;
        margin: 1.5em auto;
        padding: 1.5em;
        text-shadow: inherit;
        text-transform: uppercase;
      }
      footer p {
        margin: 0;
      }
      """
    ]

-- https://www.shadertoy.com/view/XXtBRr
balatroShader :: Text.Text
balatroShader =
  """
  precision highp float;

  uniform vec2  iResolution;
  uniform float iTime;

  #define SPIN_ROTATION -2.0
  #define SPIN_SPEED 7.0
  #define OFFSET vec2(0.0)
  #define COLOUR_1 vec4(0.871, 0.267, 0.231, 1.0)
  #define COLOUR_2 vec4(0.0, 0.42, 0.706, 1.0)
  #define COLOUR_3 vec4(0.086, 0.137, 0.145, 1.0)
  #define CONTRAST 3.5
  #define LIGTHING 0.4
  #define SPIN_AMOUNT 0.25
  #define PIXEL_FILTER 745.0
  #define SPIN_EASE 1.0
  #define PI 3.14159265359
  #define IS_ROTATE false

  vec4 effect(vec2 screenSize, vec2 screen_coords) {
      float pixel_size = length(screenSize.xy) / PIXEL_FILTER;
      vec2 uv = (floor(screen_coords.xy*(1./pixel_size))*pixel_size - 0.5*screenSize.xy)/length(screenSize.xy) - OFFSET;
      float uv_len = length(uv);

      float speed = (SPIN_ROTATION*SPIN_EASE*0.2);
      if(IS_ROTATE){
         speed = iTime * speed;
      }
      speed += 302.2;
      float new_pixel_angle = atan(uv.y, uv.x) + speed - SPIN_EASE*20.*(1.*SPIN_AMOUNT*uv_len + (1. - 1.*SPIN_AMOUNT));
      vec2 mid = (screenSize.xy/length(screenSize.xy))/2.;
      uv = (vec2((uv_len * cos(new_pixel_angle) + mid.x), (uv_len * sin(new_pixel_angle) + mid.y)) - mid);

      uv *= 30.;
      speed = iTime*(SPIN_SPEED);
      vec2 uv2 = vec2(uv.x+uv.y);

      for(int i=0; i < 5; i++) {
          uv2 += sin(max(uv.x, uv.y)) + uv;
          uv  += 0.5*vec2(cos(5.1123314 + 0.353*uv2.y + speed*0.131121),sin(uv2.x - 0.113*speed));
          uv  -= 1.0*cos(uv.x + uv.y) - 1.0*sin(uv.x*0.711 - uv.y);
      }

      float contrast_mod = (0.25*CONTRAST + 0.5*SPIN_AMOUNT + 1.2);
      float paint_res = min(2., max(0.,length(uv)*(0.035)*contrast_mod));
      float c1p = max(0.,1. - contrast_mod*abs(1.-paint_res));
      float c2p = max(0.,1. - contrast_mod*abs(paint_res));
      float c3p = 1. - min(1., c1p + c2p);
      float light = (LIGTHING - 0.2)*max(c1p*5. - 4., 0.) + LIGTHING*max(c2p*5. - 4., 0.);
      return (0.3/CONTRAST)*COLOUR_1 + (1. - 0.3/CONTRAST)*(COLOUR_1*c1p + COLOUR_2*c2p + vec4(c3p*COLOUR_3.rgb, c3p*COLOUR_1.a)) + light;
  }

  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
      vec2 uv = fragCoord/iResolution.xy;

      fragColor = effect(iResolution.xy, uv * iResolution.xy);
  }

  void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
  }
  """

-- https://raw.githubusercontent.com/xemantic/shader-web-background/73a57961b58687f5a3b2400c2b56b9aa22a56913/dist/shader-web-background.min.js
shaderWebBackground :: Text.Text
shaderWebBackground =
  """
  const shaderWebBackground={};(()=>{'use strict';const t=(a,b)=>{b.initHalfFloatRGBATexture(b.width,b.height);a.texParameteri(a.TEXTURE_2D,
  a.TEXTURE_MIN_FILTER,a.LINEAR);a.texParameteri(a.TEXTURE_2D,a.TEXTURE_MAG_FILTER,
  a.LINEAR);a.texParameteri(a.TEXTURE_2D,a.TEXTURE_WRAP_S,a.CLAMP_TO_EDGE);a.texParameteri(a.TEXTURE_2D,
  a.TEXTURE_WRAP_T,a.CLAMP_TO_EDGE)},x=(a,b)=>{console.warn("shader-web-background cannot shade, adding fallback CSS classes");
  document.documentElement.classList.add("shader-web-background-fallback");b.classList.add("shader-web-background-fallback");
  if(a instanceof shaderWebBackground.GlError)console.warn("Not sufficient WebGL support:",
  a);else throw a;};
  function y(a,b){if(!a)throw new shaderWebBackground.ConfigError(b);}
  function z(a){y(a instanceof HTMLCanvasElement,"config.canvas must be instance of canvas");
  return a}
  function A(){const a=document.createElement("canvas"),b=a.style;a.id="shader-web-background";b.width=
  "100vw";b.height="100vh";b.position="fixed";b.top="0";b.left="0";b.zIndex=-9999;return a}
  function B(a,b,c){y(a instanceof HTMLScriptElement&&a.type===b,'Shader source element of id "'+
  c+'" should be of type: <script type="'+(b+'" id="'+c+'">'))}
  function D(a){const b=document.getElementById(a);y(b,'Missing shader source: <script type="x-shader/x-fragment" id="'+
  (a+'">'));B(b,"x-shader/x-fragment",a);return b.text}
  function E(a){a+="Vertex";const b=document.getElementById(a);return b?(B(b,"x-shader/x-vertex",
  a),b.text):"attribute vec2 V;void main(){gl_Position=vec4(V,0,1);}"}
  function F(a,b){"loading"!==document.readyState?b():window.addEventListener(a,b)}
  class G{constructor(a,b,c,d){this.g=c;const l=a.gl;this.h=()=>{for(const f of d)f.u(l,
  f.location,b)};this.i=()=>{var f=c.v,h=a.gl;h.bindBuffer(h.ARRAY_BUFFER,a.j);h.enableVertexAttribArray(f);
  h.vertexAttribPointer(f,2,h.FLOAT,!1,0,0);h.drawArrays(h.TRIANGLE_STRIP,0,4);h.disableVertexAttribArray(f);
  h.bindBuffer(h.ARRAY_BUFFER,null);f=a.gl;for(h=0;h<a.g;h++)f.activeTexture(f.TEXTURE0+
  h),f.bindTexture(f.TEXTURE_2D,null);a.g=0}}}
  function H(a){var b={antialias:!1,depth:!1,alpha:!1};try{return new I(a,b)}catch(c){throw new shaderWebBackground.GlError(c.message);
  }}
  function J(a,b,c,d,l,f){function h(e,m,n){try{{var k=p;const q=k.gl,P=K(k,e,q.VERTEX_SHADER,m),
  Q=K(k,e,q.FRAGMENT_SHADER,n),v=q.createProgram();q.attachShader(v,P);q.attachShader(v,
  Q);q.linkProgram(v);var r=v}return r}catch(q){throw new shaderWebBackground.ConfigError(q.message);
  }}const p=H(a),w=[],g={gl:p.gl,canvas:a,width:0,height:0,cssPixelRatio:0,cssWidth:0,
  cssHeight:0,isOverShader:(e,m)=>{const n=a.getBoundingClientRect();return e>=n.left&&
  e<=n.right&&m>=n.top&&m<=n.bottom},toShaderX:e=>(e-a.getBoundingClientRect().left)*
  g.cssPixelRatio+.5,toShaderY:e=>a.height-(e-a.getBoundingClientRect().top)*g.cssPixelRatio-
  .5,s:()=>g.cssWidth!==a.clientWidth||g.cssHeight!==a.clientHeight?(g.resize(),!0):
  !1,resize:()=>{const e=window.devicePixelRatio||1,m=a.clientWidth,n=a.clientHeight,
  k=Math.floor(m*e),r=Math.floor(n*e);a.width=k;a.height=r;g.width=k;g.height=r;g.cssPixelRatio=
  e;g.cssWidth=m;g.cssHeight=n;p.gl.viewport(0,0,p.canvas.width,p.canvas.height);for(const q of w)q.g.l(k,
  r)},texture:(e,m)=>{{var n=p;const k=n.gl;m=m instanceof L?m.g:m;k.activeTexture(k.TEXTURE0+
  n.g);k.bindTexture(k.TEXTURE_2D,m);k.uniform1i(e,n.g++)}},buffers:{},initHalfFloatRGBATexture:(e,
  m)=>{p.h.g(e,m)}},R=Object.keys(b).length-1;let S=0;for(const e in b){if(S++<R){const k=
  b[e].texture||t;g.buffers[e]=M(p,()=>{k(p.gl,g)})}const m=N(p,h(e,E(e),D(e)),g.buffers[e]),
  n=b[e].uniforms||{};var u=Object.keys(n);for(const k of m.m)y(n[k.name],'No configuration for uniform "'+
  k.name+'" defined in shader "'+e+'"'),u=u.filter(r=>r!==k.name);0!==u.length&&console.warn('Extra uniforms configured for shader "'+
  e+'", which are not present in the shader code - might have been removed by GLSL compiler if not used: '+
  u.join(", "));u=m.m.map(k=>({location:k.location,u:n[k.name]}));w.push(new G(p,g,
  m,u))}const C=()=>{g.s()&&d&&d(g.width,g.height,g);l&&l(g);for(const e of w)e.g.i(e.h,
  e.i);f&&f(g);requestAnimationFrame(C)};F("load",()=>{g.resize();c&&c(g);d&&d(g.width,
  g.height,g);requestAnimationFrame(C)});return g}
  shaderWebBackground.Error=class extends Error{constructor(a){super(a);this.name="shaderWebBackground.Error"}};
  shaderWebBackground.ConfigError=class extends shaderWebBackground.Error{constructor(a){super(a);
  this.name="shaderWebBackground.ConfigError"}};
  shaderWebBackground.GlError=class extends shaderWebBackground.Error{constructor(a){super(a);this.name=
  "shaderWebBackground.GlError"}};
  shaderWebBackground.shade=function(a){y(a,"Missing config argument");const b=a.canvas?
  z(a.canvas):A();y(a.shaders,"No shaders specified in config");try{const c=J(b,a.shaders,
  a.onInit,a.onResize,a.onBeforeFrame,a.onAfterFrame);a.canvas||F("DOMContentLoaded",
  ()=>{document.body.appendChild(b)});return c}catch(c){(a.onError||x)(c,b)}};const O=[-1,1,1,1,-1,-1,1,-1];
  function T(a,b){return a.j(a.gl.getExtension(b),b+" extension is not supported")}
  class U{constructor(a,b){this.gl=a;this.j=b}g(){}}
  class V extends U{constructor(a,b){super(a,b);this.h=T(this,"OES_texture_half_float");
  T(this,"OES_texture_half_float_linear")}g(a,b){const c=this.gl;c.texImage2D(c.TEXTURE_2D,
  0,c.RGBA,a,b,0,c.RGBA,this.h.HALF_FLOAT_OES,null)}}
  class W extends U{constructor(a,b){super(a,b);T(this,"EXT_color_buffer_float");this.gl.getExtension("OES_texture_float_linear")}g(a,
  b){const c=this.gl;c.texImage2D(c.TEXTURE_2D,0,c.RGBA16F,a,b,0,c.RGBA,c.HALF_FLOAT,
  null)}}
  function X(a){a=a.split(/\\r?\\n/);const b=a.length.toString().length;var c=[];a.forEach((d,
  l)=>{l=(l+1).toString();l=l.length>=b?l:" ".repeat(b-l.length)+l;c.push(l+": "+d+
  "\\n")});return c.join("")}function M(a,b){return new L(a.gl,()=>{b(a.gl)})}
  function N(a,b,c){const d=a.gl;a=[];const l=d.getProgramParameter(b,d.ACTIVE_UNIFORMS);
  for(let f=0;f<l;f++){const h=d.getActiveUniform(b,f);a.push({name:h.name,location:d.getUniformLocation(b,
  h.name)})}return{v:d.getAttribLocation(b,"V"),m:a,l:c?(f,h)=>c.l(f,h):()=>{},i:(f,
  h)=>{d.useProgram(b);f();c?(f=c.g,c.g=c.h,c.h=f,c.i(h)):h()}}}
  function K(a,b,c,d){a=a.gl;c=a.createShader(c);a.shaderSource(c,d);a.compileShader(c);
  if(!a.getShaderParameter(c,a.COMPILE_STATUS)){const l=String(a.getShaderInfoLog(c));
  a.deleteShader(c);b="Cannot compile shader - "+b+": "+l;console.log(b);console.log(X(d));
  throw Error(b);}return c}
  class I{constructor(a,b){this.canvas=a;const c=(l,f)=>{if(!l)throw Error(f);return l};
  let d=a.getContext("webgl2",b);if(d)this.h=new W(d,c);else if(d=a.getContext("webgl",
  b))this.h=new V(d,c);c(d,"webgl context not supported on supplied canvas element: "+
  a);this.gl=d;a=d.createBuffer();d.bindBuffer(d.ARRAY_BUFFER,a);d.bufferData(d.ARRAY_BUFFER,
  new Float32Array(O),d.STATIC_DRAW);d.bindBuffer(d.ARRAY_BUFFER,null);this.j=a;this.buffers=
  {};this.g=0}}
  function Y(a){const b=a.gl,c=b.createTexture();b.bindTexture(b.TEXTURE_2D,c);a.o(b);
  b.bindTexture(b.TEXTURE_2D,null);return c}
  class L{constructor(a,b){this.j=a.createFramebuffer();this.gl=a;this.o=b;this.g=this.h=
  null}l(){this.h&&this.gl.deleteTexture(this.h);this.g&&this.gl.deleteTexture(this.g);
  this.h=Y(this);this.g=Y(this)}i(a){const b=this.gl;b.bindFramebuffer(b.FRAMEBUFFER,
  this.j);b.framebufferTexture2D(b.FRAMEBUFFER,b.COLOR_ATTACHMENT0,b.TEXTURE_2D,this.g,
  0);a();b.framebufferTexture2D(b.FRAMEBUFFER,b.COLOR_ATTACHMENT0,b.TEXTURE_2D,null,
  0);b.bindFramebuffer(b.FRAMEBUFFER,null)}};})()
  """

-- https://fontstruct.com/fontstructions/show/2326420/balatro
balatroFont :: Text.Text
balatroFont = "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"
