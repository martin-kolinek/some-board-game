{-# LANGUAGE OverloadedStrings #-}

module Player.Style where

import Player.Building.Style
import Common.CssClass
import Common.CommonClasses

import Clay
import Clay.Flexbox as F
import Data.Monoid

playerClass :: CssClass
playerClass = CssClass "player"
selectedPlayerClass :: CssClass
selectedPlayerClass = CssClass "selected-player"
playerContainerClass :: CssClass
playerContainerClass = CssClass "player-container"
selectionClass :: CssClass
selectionClass = CssClass "selection"
playerDataContainerClass :: CssClass
playerDataContainerClass = CssClass "player-data-container"
hiddenPlayerData :: CssClass
hiddenPlayerData = CssClass "hidden-player-data"

freeWorkersClass :: CssClass
freeWorkersClass = CssClass "free-workers"
playerInfoClass :: CssClass
playerInfoClass = CssClass "player-info"
currentPlayerIconClassInternal :: CssClass
currentPlayerIconClassInternal = CssClass "current-player-icon"
armingClass :: CssClass
armingClass = CssClass "arming"
currentPlayerIconClass :: CssClass
currentPlayerIconClass = currentPlayerIconClassInternal <> faClass <> faPlayClass

playerStyle :: Css
playerStyle = do
  buildingStyle
  star # classSelector playerDataContainerClass ? do
    display Clay.flex
    flexFlow row F.wrap
    margin nil nil nil nil
    padding nil nil nil nil
  star # classSelector hiddenPlayerData ? do
    display none
  star # classSelector freeWorkersClass ? do
    position fixed
    bottom (em 2)
    right (em 2)
    padding (em 1) (em 1) (em 1) (em 1)
    width (pct 45)
    height (pct 55)
    boxSizing borderBox
  star # classSelector playerClass ? do
    display inlineBlock
    margin nil (em 0.3) (em 0.3) (em 0.3)
    padding (em 0.7) (em 0.7) (em 0.7) (em 0.7)
    backgroundColor "#8f765f"
    borderBottomLeftRadius (px 10) (px 10)
    borderBottomRightRadius (px 10) (px 10)
    cursor pointer
    width (em 10)
  star # classSelector playerClass # firstChild ? marginLeft (em 3)
  star # classSelector playerClass # classSelector selectedPlayerClass ? do
    backgroundColor "#27201a"
    color white
    fontWeight bold
  star # classSelector playerContainerClass ? do
    order 1
    F.flex 1 1 (pct 85)
    minWidth (em 35)
  star # classSelector playerInfoClass ? do
    order 3
    F.flex 1 1 (pct 10)
    minWidth (em 10)
    padding (em 2) (em 2) (em 2) (em 2)
    margin (em 2) (em 2) (em 2) (em 2)
    fontWeight bold
  star # classSelector playerInfoClass |> button ? do
    marginTop (em 1)
    display block
  star # classSelector armingClass ? do
    marginTop (em 1)
  star # classSelector armingClass Clay.** input ? do
    marginRight (em 0.5)
    width (em 4)
  star # classSelector currentPlayerIconClassInternal ?
    float floatRight
  star # classSelector selectionClass ? do
    position fixed
    display Clay.flex
    F.justifyContent center
    F.alignItems center
    left (pct 30)
    top (pct 40)
    width (pct 30)
    height(em 10)
    minWidth (em 25)
    backgroundColor "#8f765f"
    zIndex 10000
    borderRadius (em 2) (em 2) (em 2) (em 2)
  star # classSelector selectionClass Clay.** button ? do
    margin (em 2) (em 2) (em 2) (em 2)
    width (em 10)
    height (em 3)
