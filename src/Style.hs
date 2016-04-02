{-# LANGUAGE OverloadedStrings #-}

module Style where

import Common.CssClass
import Errors.Style
import Board.Style

import Clay
import Data.ByteString.Lazy hiding (repeat)
import Data.Text.Lazy.Encoding
import Prelude hiding (div, repeat)

finishClass = CssClass "finish"

mainStyle :: Css
mainStyle = do
  errorStyle
  boardStyle
  importUrl "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
  body ? do
    backgroundImage $ url "data/background.png"
    backgroundRepeat repeat
  div ? margin nil nil nil nil
  star # classSelector finishClass ? do
    position fixed
    right (px 0)
    top (px 0)
    backgroundColor green
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    minWidth (em 4)
    textAlign $ alignSide sideRight

mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
