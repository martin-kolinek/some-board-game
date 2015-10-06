{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Clay
import           CssClass
import           Data.ByteString.Lazy
import           Data.Text.Lazy.Encoding
import           Prelude                 hiding (div)

mainStyle :: Css
mainStyle = do
  star # classSelector scoreClass ? do
    position fixed
    right (px 0)
    top (px 0)
    backgroundColor green
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    minWidth (em 4)
    textAlign $ alignSide sideRight
  star # classSelector freeWorkersClass ? do
    position fixed
    bottom (px 0)
    left (pct 50)
    marginLeft (em (-10))
    padding (em 0.5) (em 0.5) (em 0.2) (em 0.5)
    backgroundColor lightgrey
    width (em 20)
    height (em 2.7)
  star # classSelector cardWrapperClass ? do
    width $ em 8
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
  star # classSelector errorContainerClass ? do
    position fixed
    width (em 40)
    left (pct 50)
    top (px 0)
    marginLeft (em (-20))
    backgroundColor salmon

mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
