module Daylatro.Type.Seed where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Formatting as F
import qualified Lucid as Html
import qualified System.Random as Random

newtype Seed
  = MkSeed String
  deriving (Eq, Show)

instance Html.ToHtml Seed where
  toHtml = Html.toHtml . toString
  toHtmlRaw = Html.toHtmlRaw . toString

toString :: Seed -> String
toString (MkSeed x) = x

toText :: Seed -> Text.Text
toText = Text.pack . toString

format :: F.Format t (Seed -> t)
format = F.mapf toString F.string

fromDay :: Time.Day -> Seed
fromDay =
  MkSeed
    . fmap (toEnum . (\x -> x + if x < 10 then 48 else 55))
    . fst
    . Random.uniformListR 8 (0, 35)
    . Random.mkStdGen
    . fromIntegral
    . Time.toModifiedJulianDay
