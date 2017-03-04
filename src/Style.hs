{-# LANGUAGE OverloadedStrings #-}

module Style where

import Errors.Style
import Player.Board.Style

import Clay
import Data.ByteString.Lazy hiding (repeat, ByteString)
import Data.ByteString (ByteString)
import Data.Text.Lazy.Encoding
import Prelude hiding (div, repeat)

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

mainStyleByteString :: ByteString
mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
