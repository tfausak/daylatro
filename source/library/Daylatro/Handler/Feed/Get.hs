{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Handler.Feed.Get where

import qualified Data.Default as Default
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Daylatro.Constant.Balatro as Balatro
import qualified Daylatro.Type.Context as Context
import qualified Daylatro.Type.Seed as Seed
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as X

handler ::
  Context.Context ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
handler context respond = do
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
                        X.elementNodes = [X.NodeContent $ F.sformat (F.stext % "/feed.atom") (Context.baseUrl context)]
                      }
                    : X.NodeElement
                      X.Element
                        { X.elementName = "link",
                          X.elementAttributes =
                            Map.fromList
                              [ ("rel", "self"),
                                ("href", F.sformat (F.stext % "/feed.atom") (Context.baseUrl context))
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
                              url = F.sformat (F.stext % "/?day=" % F.dateDash) (Context.baseUrl context) day
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
                                              X.elementNodes = [X.NodeContent $ Seed.value seed]
                                            }
                                      ]
                                  }
                      )
                      [Balatro.epoch .. today]
              },
          X.documentEpilogue = []
        }
