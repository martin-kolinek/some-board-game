module Board.Player.Style where

import Common.CssClass

import Clay

playerClass = CssClass "player"
selectedPlayerClass = CssClass "selected-player"
currentPlayerClass = CssClass "current-player"
playerContainerClass = CssClass "player-container"

freeWorkersClass = CssClass "free-workers"

playerStyle = do
  star # classSelector freeWorkersClass ? do
    position fixed
    bottom (px 0)
    left (pct 50)
    marginLeft (em (-20))
    padding (em 0.5) (em 1) (em 0.5) (em 1)
    backgroundColor lightgrey
    width (em 40)
    height (em 5)
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
