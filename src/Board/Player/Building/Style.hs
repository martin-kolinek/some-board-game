{-# LANGUAGE OverloadedStrings #-}

module Board.Player.Building.Style where

import Common.CssClass
import Common.CommonClasses

import Clay
import qualified Clay.Flexbox as F
import Data.Monoid
import Rules

buildingSpaceClass = CssClass "building-space"
rotateButtonClassInternal = CssClass "rotate-button"
rotateButtonClass = rotateButtonClassInternal <> faClass <> faRotateRightClass
rotateButtonWrapperClass = CssClass "rotate-button-wrapper"
cancelButtonClassInternal = CssClass "cancel-button"
cancelButtonClassIcon = CssClass "fa fa-times"
cancelButtonClass = cancelButtonClassInternal <> faClass <> faTimesClass
cancelButtonWrapperClass = CssClass "cancel-button-wrapper"

occupantErrorClass = CssClass "occupant-error"
occupantErrorIconClass = faClass <> faExclamationClass
occupantErrorTextClass = CssClass "occupant-error-text"

occupantContainerClass = CssClass "occupant-container"

placeholderTileClass = CssClass "placeholder-tile"

buildingStyle = do
  star # classSelector buildingSpaceClass ? do
    F.flex 4 1 (pct 50)
    F.order 4
    F.alignSelf flexStart
    maxWidth (em 70)
    minWidth (em 40)
    position relative
  star # classSelector buildingSpaceClass # before ? do
    paddingBottom (pct 75)
    display block
    content $ stringContent ""
  star # classSelector rotateButtonClassInternal ?
    fontSize (em 2)
  star # classSelector rotateButtonWrapperClass ? do
    position absolute
    left (pct 6)
    top (pct 88)
    cursor pointer
  star # classSelector cancelButtonClassInternal ?
    fontSize (em 2)
  star # classSelector cancelButtonWrapperClass ? do
    position absolute
    left (pct 12)
    top (pct 88)
    cursor pointer
  star # classSelector placeholderTileClass ? do
    textAlign (alignSide sideCenter)
  star # classSelector occupantContainerClass ? do
    verticalAlign middle
    display inlineBlock
    paddingTop (pct 15)
    height (pct 85)
    overflow auto
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
    zIndex 10
  star # classSelector occupantErrorClass # hover ? do
    width (pct 100)
    height auto
    fontSize (px 12)
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    fontFamily [] [sansSerif]
  star # classSelector occupantErrorClass |> Clay.div # classSelector occupantErrorTextClass ?
    display none
  star # classSelector occupantErrorClass # hover |> Clay.div # classSelector occupantErrorTextClass ?
    display inline
  star # classSelector occupantErrorClass |> Clay.div # classSelector faExclamationClass ?
    display inline
  star # classSelector occupantErrorClass # hover |> Clay.div # classSelector faExclamationClass ?
    display none

oneSixth = 15
oneQuarter = 20

buildingCss (Grass position) = background (url "data/grass.svg") >> positionCss position >> commonBuildingCss
buildingCss (Forest position) = background (url "data/forest.svg") >> positionCss position >> commonBuildingCss
buildingCss (Rock position) = background (url "data/rock.svg") >> positionCss position >> commonBuildingCss
buildingCss (InitialRoom position) = background (url "data/room.svg") >> positionCss position >> commonBuildingCss

commonBuildingCss = width (pct oneSixth) >> height (pct oneQuarter) >> position absolute >> backgroundSize cover

positionCss (x, y) = left (pct $ (fromIntegral x)*oneSixth + 5) >> top (pct $ (fromIntegral y)*oneQuarter+5)

placeholderTileCss position = positionCss position >> commonBuildingCss

highlightedPlaceholderTileCss position = placeholderTileCss position >> backgroundColor "#FFCCCC"

highlightedValidPlaceholderTileCss position = placeholderTileCss position >> backgroundColor "#CCFFCC"
