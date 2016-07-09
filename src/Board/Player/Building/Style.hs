{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Building.Style where

import Common.CssClass

import Clay
import Data.Monoid
import Rules

buildingSpaceClass = CssClass "building-space"
rotateButtonClassInternal = CssClass "rotate-button"
rotateButtonClassIcon = CssClass "fa fa-rotate-right"
rotateButtonClass = rotateButtonClassInternal <> rotateButtonClassIcon
rotateButtonWrapperClass = CssClass "rotate-button-wrapper"
cancelButtonClassInternal = CssClass "cancel-button"
cancelButtonClassIcon = CssClass "fa fa-times"
cancelButtonClass = cancelButtonClassInternal <> cancelButtonClassIcon
cancelButtonWrapperClass = CssClass "cancel-button-wrapper"

occupantErrorClass = CssClass "occupant-error"
occupantErrorIconClass = CssClass "fa fa-exclamation"
occupantErrorTextClass = CssClass "occupant-error-text"

occupantContainerClass = CssClass "occupant-container"

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
  star # classSelector occupantContainerClass ? do
    marginTop (px 30)
    marginLeft (px 30)
  star # classSelector occupantErrorClass ? do
    position absolute
    backgroundColor red
    margin (px 8) (px 8) (px 8) (px 8)
    width (px 25)
    height (px 25)
    borderRadius (px 15) (px 15) (px 15) (px 15)
    textAlign $ alignSide sideCenter
    verticalAlign middle
    lineHeight (px 27)
  star # classSelector occupantErrorClass # hover ? do
    width auto
    height auto
    fontSize (px 12)
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    fontFamily [] [sansSerif]
  star # classSelector occupantErrorClass |> Clay.div # classSelector occupantErrorTextClass ?
    display none
  star # classSelector occupantErrorClass # hover |> Clay.div # classSelector occupantErrorTextClass ?
    display inline
  star # classSelector occupantErrorClass |> Clay.div # ".fa-exclamation" ?
    display inline
  star # classSelector occupantErrorClass # hover |> Clay.div # ".fa-exclamation" ?
    display none

buildingCss (Grass position) = backgroundColor lightgreen >> positionCss position >> commonBuildingCss
buildingCss (Forest position) = backgroundColor darkgreen >> positionCss position >> commonBuildingCss
buildingCss (Rock position) = backgroundColor gray >> positionCss position >> commonBuildingCss
buildingCss (InitialRoom position) = backgroundColor red >> positionCss position >> commonBuildingCss

commonBuildingCss = width (em 12) >> height (em 12) >> position absolute >> borderWidth 1 >> borderStyle solid

positionCss (x, y) = left (em $ fromIntegral x*12) >> top (em $ fromIntegral y*12)

placeholderTileCss position = positionCss position >> commonBuildingCss

highlightedPlaceholderTileCss position = placeholderTileCss position >> backgroundColor "#FFCCCC"

highlightedValidPlaceholderTileCss position = placeholderTileCss position >> backgroundColor "#CCFFCC"
