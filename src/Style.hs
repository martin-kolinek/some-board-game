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
    padding (em 0.5) (em 1) (em 0.5) (em 1)
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
    padding (em 1) (em 1) (em 1) (em 1)
    background $ url "data/card.svg"
    backgroundSize contain
    backgroundRepeat noRepeat
  let workerFade = do
        opacity 0
        width (px 0)
        margin (px 0) (px 0) (px 0) (px 0)
        transitions [("opacity", sec 0.5, easeInOut, sec 0), ("width", sec 0.5, easeInOut, sec 0.5), ("margin", sec 0.5, easeInOut, sec 0.5)]
      workerCommon = do
        opacity 1
        width (em 4)
        margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)
        transitions [("width", sec 0.5, easeInOut, sec 0), ("margin", sec 0.5, easeInOut, sec 0), ("opacity", sec 0.5, easeInOut, sec 0.5)]
        display inlineBlock
        height $ em 4
        backgroundSize contain
        backgroundRepeat noRepeat
        animationName "worker-kf"
        animationDuration (sec 1)
        animationIterationCount (iterationCount 1)
  keyframes "worker-kf" [
      (0, opacity 0),
      (100, opacity 1)]
  star # classSelector workerClass ? do
    background $ url "data/worker.svg"
    workerCommon
  star # classSelector activeWorkerClass ? do
    background (url "data/worker_glowing.svg")
    workerCommon
  star # classSelector workerClass # classSelector fadeClass ? workerFade
  star # classSelector activeWorkerClass # classSelector fadeClass ? workerFade
  star # classSelector errorContainerClass ? do
    position fixed
    width (em 40)
    left (pct 50)
    top (px 0)
    marginLeft (em (-20))
  keyframes "error-kf" [
    (0, height nil),
    (100, height (em 2))]
  star # classSelector errorItemClass ? do
    width (pct 100)
    backgroundImage $ linearGradient (straight sideTop) [("#f2cece", pct 0), ("#e7b3b3", pct 100)]
    backgroundRepeat repeatX
    borderColor "#dc9797"
    borderRadius (px 4) (px 4) (px 4) (px 4)
    height (em 2)
    borderWidth (px 1)
    borderStyle solid
    overflow hidden
    transition "height" (sec 1) ease (sec 0)
    verticalAlign middle
    animationName "error-kf"
    animationIterationCount (iterationCount 1)
    animationDuration (sec 1)
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
  star # classSelector playerClass ? do
    margin (em 0.3) (em 0.3) (em 0.3) (em 0.3)
    padding (em 0.3) (em 0.3) (em 0.3) (em 0.3)
    backgroundColor lightblue
    cursor pointer
  star # classSelector playerClass # classSelector selectedPlayerClass ? do
    backgroundColor darkblue
    color white
  star # classSelector playerClass # classSelector currentPlayerClass ?
    fontWeight bold
  star # classSelector playerContainerClass ? do
    position fixed
    right nil
    top (pct 50)
    marginTop (em (-6))
    height (em 12)

mainStyleByteString = toStrict $ encodeUtf8 $ render mainStyle
