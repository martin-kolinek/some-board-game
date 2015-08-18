{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Clay
import           CssClass
import           Data.ByteString.Lazy
import           Data.Text.Lazy.Encoding
import           Prelude                 hiding (div)

mainStyle :: Css
mainStyle = do
  star # classSelector cardWrapperClass ? do
    width $ em 5
    display inlineBlock
    position relative
    margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)
  star # classSelector cardWrapperClass # after ? do
    paddingTop $ pct 143.28
    display block
    content $ stringContent ""
  star # classSelector cardClass ? do
    position absolute
    top $ px 0
    bottom $ px 0
    left $ px 0
    right $ px 0
    background $ url "data/card.svg"
    backgroundSize contain
    backgroundRepeat noRepeat
  star # classSelector workerClass ? do
    display inlineBlock
    width $ em 2
    height $ em 2
    margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    background $ url "data/worker.svg"
    backgroundSize contain
    backgroundRepeat noRepeat

mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
