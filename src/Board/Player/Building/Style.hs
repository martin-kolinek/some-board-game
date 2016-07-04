{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Building.Style where

import Common.CssClass

import Clay
import Data.Monoid

buildingSpaceClass = CssClass "building-space"
rotateButtonClassInternal = CssClass "rotate-button"
rotateButtonClassIcon = CssClass "fa fa-rotate-right"
rotateButtonClass = rotateButtonClassInternal <> rotateButtonClassIcon
rotateButtonWrapperClass = CssClass "rotate-button-wrapper"
cancelButtonClassInternal = CssClass "cancel-button"
cancelButtonClassIcon = CssClass "fa fa-times"
cancelButtonClass = cancelButtonClassInternal <> cancelButtonClassIcon
cancelButtonWrapperClass = CssClass "cancel-button-wrapper"

buildingStyle = do
  star # classSelector buildingSpaceClass ? do
    position fixed
    bottom (em 2)
    right (em 2)
    padding (em 1) (em 1) (em 1) (em 1)
    width (pct 45)
    height (pct 55)
    boxSizing borderBox
  star # classSelector rotateButtonClassInternal ?
    fontSize (px 30)
  star # classSelector rotateButtonWrapperClass ? do
    position absolute
    left (em 50)
    top (em 2)
    cursor pointer
  star # classSelector cancelButtonClassInternal ?
    fontSize (px 30)
  star # classSelector cancelButtonWrapperClass ? do
    position absolute
    left (em 50)
    top (em 6)
    cursor pointer
