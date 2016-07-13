{-# LANGUAGE OverloadedStrings #-}

module Style where

import Common.CssClass
import Errors.Style
import Board.Style

import Clay
import Data.ByteString.Lazy hiding (repeat)
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
  div ? margin nil nil nil nil
  
mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
