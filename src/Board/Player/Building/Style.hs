{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Building.Style where

import Common.CssClass

import Clay

buildingSpaceClass = CssClass "building-space"

buildingStyle = star # classSelector buildingSpaceClass ? do
  position fixed
  bottom (em 2)
  right (em 2)
  padding (em 1) (em 1) (em 1) (em 1)
  width (pct 45)
  height (pct 55)
  boxSizing borderBox
