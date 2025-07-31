module Daylatro.Type.Seed where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Formatting as F
import qualified Lucid as Html
import qualified System.Random as Random

newtype Seed
  = MkSeed Text.Text
  deriving (Eq, Show)

instance Html.ToHtml Seed where
  toHtml = Html.toHtml . toText
  toHtmlRaw = Html.toHtmlRaw . toText

toText :: Seed -> Text.Text
toText (MkSeed x) = x

format :: F.Format t (Seed -> t)
format = F.mapf toText F.stext

fromDay :: Time.Day -> Seed
fromDay =
  MkSeed
    . Text.pack
    . fmap (toEnum . (\x -> x + if x < 10 then 48 else 55))
    . fst
    . Random.uniformListR 8 (0, 35)
    . Random.mkStdGen
    . fromIntegral
    . Time.toModifiedJulianDay
