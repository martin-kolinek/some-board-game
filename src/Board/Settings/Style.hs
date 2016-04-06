module Board.Settings.Style where

import Common.CssClass

import Data.Monoid
import Clay

settingsIconClassSymbol = CssClass "fa fa-cog"
settingsIconClassInternal = CssClass "settings-icon"
settingsIconClass = settingsIconClassSymbol <> settingsIconClassInternal

settingsPopupClass = CssClass "settings-popup"

settingsPopupCloseIcon = CssClass "fa fa-times"
settingsPopupCloseInternal = CssClass "settings-popup-close"
settingsPopupClose = settingsPopupCloseIcon <> settingsPopupCloseInternal

shroudClass = CssClass "shroud"

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
    marginTop (em (-10))
    height (em 20)
    marginLeft (em (-20))
    width (em 40)
    backgroundColor red
    zIndex 2
  star # classSelector settingsPopupCloseInternal ? do
    position absolute
    right (pt 5)
    top (pt 5)
    fontSize (pt 15)
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
