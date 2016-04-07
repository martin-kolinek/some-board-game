{-# LANGUAGE OverloadedStrings #-}

module Board.Settings.Style where

import Common.CssClass

import Data.Monoid
import Clay
import Prelude hiding ((**))

settingsIconClassSymbol = CssClass "fa fa-cog"
settingsIconClassInternal = CssClass "settings-icon"
settingsIconClass = settingsIconClassSymbol <> settingsIconClassInternal

settingsPopupClass = CssClass "settings-popup"

settingsPopupCloseIcon = CssClass "fa fa-times"
settingsPopupCloseInternal = CssClass "settings-popup-close"
settingsPopupClose = settingsPopupCloseIcon <> settingsPopupCloseInternal

shroudClass = CssClass "shroud"

settingsLineClass = CssClass "settings-line"

boardSettingsStyle = do
  star # classSelector settingsIconClassInternal ? do
    position fixed
    right (pt 10)
    top (pt 5)
    fontSize (pt 30)
    cursor pointer
  star # classSelector settingsPopupClass ? do
    position fixed
    left (pct 50)
    top (pct 50)
    marginTop (em (-15))
    height (em 30)
    marginLeft (em (-24))
    width (em 48)
    backgroundImage $ url "data/background.png"
    borderWidth (px 3)
    borderStyle solid
    borderColor "#222222"
    padding (em 3) (em 3) (em 3) (em 3)
    borderRadius (px 15) (px 15) (px 15) (px 15)
    zIndex 2
  star # classSelector settingsPopupCloseInternal ? do
    position absolute
    right (pt 10)
    top (pt 10)
    fontSize (pt 20)
    cursor pointer
  star # classSelector shroudClass ? do
    zIndex 1
    position fixed
    width (pct 100)
    height (pct 100)
    left nil
    top nil
    margin nil nil nil nil
    opacity 0.6
    backgroundColor black
  star # classSelector settingsLineClass ? do
    height (em 4.4)
    lineHeight (em 4.4)
    margin (em 0.8) (em 0.8) (em 0.8) (em 0.8)
  star # classSelector settingsLineClass ** input ? do
    verticalAlign middle
    marginRight (em 3)
