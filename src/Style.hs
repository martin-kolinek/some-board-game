{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Clay
import           CssClass
import           Data.ByteString.Lazy
import           Data.Text.Lazy.Encoding
import           Prelude                 hiding (div)

mainStyle :: Css
mainStyle = do
  importUrl "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
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
    marginLeft (em (-20))
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    backgroundColor lightgrey
    width (em 40)
    height (em 5)
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
    height $ em 4
    background $ url "data/worker.svg"
    backgroundSize contain
    backgroundRepeat noRepeat
  star # classSelector workerClass# classSelector fadeClass ? do
    opacity 0
    width (px 0)
    margin (px 0) (px 0) (px 0) (px 0)
    transitions [("opacity", sec 0.5, easeInOut, sec 0), ("width", sec 0.5, easeInOut, sec 0.5), ("margin", sec 0.5, easeInOut, sec 0.5)]
  star # classSelector workerClass # classSelector appearClass ? do
    opacity 1
    width (em 4)
    margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    transitions [("width", sec 0.5, easeInOut, sec 0), ("margin", sec 0.5, easeInOut, sec 0), ("opacity", sec 0.5, easeInOut, sec 0.5)]
  star # classSelector errorContainerClass ? do
    position fixed
    width (em 40)
    left (pct 50)
    top (px 0)
    marginLeft (em (-20))
  star # classSelector errorItemClass ? do
    width (pct 100)
    display inlineBlock
    backgroundImage $ linearGradient (straight sideTop) [("#f2cece", pct 0), ("#e7b3b3", pct 100)]
    backgroundRepeat repeatX
    borderColor "#dc9797"
    borderRadius (px 4) (px 4) (px 4) (px 4)
    borderWidth (px 1)
    borderStyle solid
    overflow hidden
    transition "height" (sec 1) ease (sec 0)
    verticalAlign middle
  star # classSelector errorItemClass |> div ? do
    marginTop (em 0.5)
    marginBottom (em 0.5)
    display inlineBlock
  star # classSelector errorItemClass |> div # lastChild ? do
    float floatRight
    marginRight (em 0.3)
  star # classSelector errorItemClass |> div # firstChild ? do
    fontWeight bold
    marginLeft (em 0.3)
  star # classSelector errorItemClass # classSelector fadeClass ?
    height (em 0)
  star # classSelector errorItemClass # classSelector appearClass ?
    height (em 2)


mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
