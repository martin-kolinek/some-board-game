{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Style where

import Common.CssClass
import Data.Monoid

import Clay

playerClass = CssClass "player"
selectedPlayerClass = CssClass "selected-player"
playerContainerClass = CssClass "player-container"

freeWorkersClass = CssClass "free-workers"
resourcesClass = CssClass "resources"
playClass = CssClass "fa fa-play"
currentPlayerIconClass = CssClass "current-player-icon"
currentPlayerClass = playClass <> currentPlayerIconClass

playerStyle = do
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
  star # classSelector playerClass # classSelector selectedPlayerClass ? do
    backgroundColor "#27201a"
    color white
    fontWeight bold
  star # classSelector playerContainerClass ? do
    position fixed
    right (em 6)
    top nil
  star # classSelector resourcesClass ? do
    position fixed
    right (em 2)
    top (em 4)
    width (pct 45)
    height (pct 30)
    padding (em 1) (em 1) (em 1) (em 1)
    boxSizing borderBox
    fontWeight bold
  star # classSelector currentPlayerIconClass ?
    float floatRight
