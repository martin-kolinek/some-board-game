{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Style where

import Board.Player.Building.Style
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

freeWorkersClass :: CssClass
freeWorkersClass = CssClass "free-workers"
resourcesClass :: CssClass
resourcesClass = CssClass "resources"
currentPlayerIconClassInternal :: CssClass
currentPlayerIconClassInternal = CssClass "current-player-icon"
currentPlayerIconClass :: CssClass
currentPlayerIconClass = currentPlayerIconClassInternal <> faClass <> faPlayClass

playerStyle :: Css
playerStyle = do
  buildingStyle
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
  star # classSelector resourcesClass ? do
    order 3
    F.flex 1 1 (pct 10)
    minWidth (em 10)
    padding (em 2) (em 2) (em 2) (em 2)
    margin (em 2) (em 2) (em 2) (em 2)
    fontWeight bold
  star # classSelector currentPlayerIconClassInternal ?
    float floatRight
