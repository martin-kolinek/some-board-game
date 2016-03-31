{-# LANGUAGE OverloadedStrings #-}

module Errors.Style where

import Common.CssClass hiding (fadeClass)
import qualified Common.CssClass as C
import Data.Monoid
import Clay
import Prelude hiding (div)

errorContainerClass = CssClass "error-container"
errorItemClass = CssClass "error-item"
closeButtonClass = CssClass "close-button"
closeButtonWithIconClass = CssClass "fa fa-times" <> closeButtonClass
fadeClass = C.fadeClass

errorStyle = do
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
  star # classSelector closeButtonClass ? cursor pointer
