{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Handler.Index.Post where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Daylatro.Extra.Ord as Ord
import qualified Daylatro.Handler.Common as Common
import qualified Daylatro.Type.Context as Context
import qualified Daylatro.Type.Score as Score
import Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Read as Read

handler ::
  Context.Context ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
handler context request respond = do
  now <- Time.getCurrentTime
  query <- Http.parseQueryText . LazyByteString.toStrict <$> Wai.consumeRequestBodyLazy request
  let maybeScore = do
        day <-
          Common.parseDay
            . Maybe.fromMaybe ""
            . Monad.join
            $ lookup "day" query
        name <- fmap (Text.map Char.toUpper) . Monad.join $ lookup "name" query
        Monad.guard . Ord.between 1 3 $ Text.length name
        Monad.guard $ Text.all (\c -> Ord.between 'A' 'Z' c || Ord.between '0' '9' c) name
        ante <-
          Read.readMaybe
            . maybe "" Text.unpack
            . Monad.join
            $ lookup "ante" query
        Monad.guard $ Ord.between 0 39 ante
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
    Nothing -> respond $ Common.statusResponse Http.badRequest400 []
    Just score -> do
      Sql.execute
        (Context.connection context)
        """
        insert into Score (createdAt, day, name, ante, bestHand)
        values (?, ?, ?, ?, ?)
        """
        score
      respond $
        Common.statusResponse
          Http.found302
          [(Http.hLocation, F.formatted Encoding.encodeUtf8 ("/?day=" % F.dateDash) (Score.day score))]
