{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Daylatro.Constant.Style where

import qualified Data.Text as Text
import qualified Daylatro.Constant.Font as Font
import Formatting ((%))
import qualified Formatting as F

daylatro :: Text.Text
daylatro =
  F.sformat
    ( "@font-face { font-family: 'Balatro'; src: url(data:text/woff;base64,"
        % F.stext
        % "); }"
        % """
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
    )
    Font.balatro
