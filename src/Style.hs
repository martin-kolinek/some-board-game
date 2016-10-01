{-# LANGUAGE OverloadedStrings #-}

module Style where

import Common.CssClass
import Errors.Style
import Board.Style

import Clay
import qualified Clay.Flexbox as F
import Data.ByteString.Lazy hiding (repeat, ByteString)
import Data.ByteString (ByteString)
import Data.Text.Lazy.Encoding
import Prelude hiding (div, repeat)

wrapperClass :: CssClass
wrapperClass = CssClass "wrapper"

mainStyle :: Css
mainStyle = do
  errorStyle
  boardStyle
  importUrl "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
  body ? do
    backgroundImage $ url "data/background.png"
    backgroundRepeat repeat
    fontFamily [] [sansSerif]
    fontSize (px 12)
    margin nil nil nil nil
    padding nil nil nil nil
  div ? margin nil nil nil nil
  star # classSelector wrapperClass ? do
    display flex
    flexFlow row F.wrap
    margin nil nil nil nil
    padding nil nil nil nil

mainStyleByteString :: ByteString
mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
