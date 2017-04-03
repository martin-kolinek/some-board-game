{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Player.Building.Style where

import Common.CssClass
import Common.CommonClasses

import Clay
import qualified Clay.Flexbox as F
import Data.Monoid
import Rules

buildingSpaceClass :: CssClass
buildingSpaceClass = CssClass "building-space"
rotateButtonClassInternal :: CssClass
rotateButtonClassInternal = CssClass "rotate-button"
rotateButtonClass :: CssClass
rotateButtonClass = rotateButtonClassInternal <> faClass <> faRotateRightClass
hiddenButtonClass :: CssClass
hiddenButtonClass = CssClass "hidden-button"
buildingOptionsClass :: CssClass
buildingOptionsClass = CssClass "building-options"
cancelButtonClassInternal :: CssClass
cancelButtonClassInternal = CssClass "cancel-button"
cancelButtonClassIcon :: CssClass
cancelButtonClassIcon = CssClass "fa fa-times"
cancelButtonClass :: CssClass
cancelButtonClass = cancelButtonClassInternal <> faClass <> faTimesClass
cancelButtonWrapperClass :: CssClass
cancelButtonWrapperClass = CssClass "cancel-button-wrapper"

switchBuildingLeftClassInternal  :: CssClass
switchBuildingLeftClassInternal = CssClass "switch-left"
switchBuildingLeftClass :: CssClass
switchBuildingLeftClass = switchBuildingLeftClassInternal <> faClass <> faCaretLeftClass
switchBuildingRightClassInternal  :: CssClass
switchBuildingRightClassInternal = CssClass "switch-right"
switchBuildingRightClass :: CssClass
switchBuildingRightClass = switchBuildingRightClassInternal <> faClass <> faCaretRightClass

buildButtonClass :: CssClass
buildButtonClass = CssClass "build-button"

occupantErrorClass :: CssClass
occupantErrorClass = CssClass "occupant-error"
occupantErrorIconClass :: CssClass
occupantErrorIconClass = faClass <> faExclamationClass
occupantErrorTextClass :: CssClass
occupantErrorTextClass = CssClass "occupant-error-text"

occupantContainerClass :: CssClass
occupantContainerClass = CssClass "occupant-container"

placeholderTileClass :: CssClass
placeholderTileClass = CssClass "placeholder-tile"

cropTypeClass :: CssClass
cropTypeClass = CssClass "crop-type"
highlightedCropTypeClass :: CssClass
highlightedCropTypeClass = CssClass "highlighted-crop-type"
plantCropsButtonClass :: CssClass
plantCropsButtonClass = CssClass "plant-crops-button"

buildingStyle :: Css
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
  star # classSelector hiddenButtonClass ? do
    display none
  star # classSelector rotateButtonClassInternal ? do
    cursor pointer
    display inlineBlock
    height (em 4)
    lineHeight (em 4)
    verticalAlign middle
    marginLeft (em 1)
    marginRight (em 1)
  star # classSelector buildingOptionsClass ? do
    position absolute
    fontSize (em 2)
    left (pct 6)
    top (pct 88)
  star # classSelector switchBuildingLeftClassInternal ? do
    cursor pointer
    height (em 4)
    display inlineBlock
    lineHeight (em 4)
    verticalAlign middle
    marginLeft (em 1)
    marginRight (em 0.5)
  star # classSelector switchBuildingRightClassInternal ? do
    cursor pointer
    height (em 4)
    display inlineBlock
    lineHeight (em 4)
    verticalAlign middle
    marginLeft (em 0.5)
    marginRight (em 1)
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
    paddingTop (pct 25)
    paddingLeft (pct 10)
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
  star # classSelector buildButtonClass ? do
    display inlineBlock
    borderWidth (px 1)
    borderStyle solid
    borderColor black
    cursor pointer
    marginRight (em 1)
  star # classSelector plantCropsButtonClass ? do
    display inlineBlock
    borderWidth (px 1)
    borderStyle solid
    borderColor black
    cursor pointer
    marginRight (em 1)
  star # classSelector cropTypeClass ? do
    display inlineBlock
    borderWidth (px 1)
    borderStyle solid
    borderColor black
    cursor pointer
    marginRight (em 1)
  star # classSelector highlightedCropTypeClass ? do
    backgroundColor red

oneSixth :: Double
oneSixth = 15
oneQuarter :: Double
oneQuarter = 20

buildingTypeCss :: BuildingType -> Css
buildingTypeCss Grass = background (url "data/grass.svg")
buildingTypeCss Forest = background (url "data/forest.svg")
buildingTypeCss Rock = background (url "data/rock.svg")
buildingTypeCss InitialRoom = background (url "data/init_room.svg")
buildingTypeCss Cave = background (url "data/cave.svg")
buildingTypeCss Passage = background (url "data/passage.svg")
buildingTypeCss LivingRoom = background (url "data/living_room.svg")
buildingTypeCss Field = background (url "data/field.svg")

buildingCss :: BuildingType -> Rules.Position -> Css
buildingCss buildingType position = buildingTypeCss buildingType >> positionCss position >> commonBuildingCss

commonBuildingCss :: Css
commonBuildingCss = width (pct oneSixth) >> height (pct oneQuarter) >> position absolute >> backgroundSize cover

buildingSelectionCss :: BuildingType -> Css
buildingSelectionCss buildingType = do
  buildingTypeCss buildingType
  width (em 4)
  height (em 4)
  backgroundSize cover
  display inlineBlock
  verticalAlign middle
  lineHeight (em 4)

positionCss :: (Int, Int) -> Css
positionCss (x, y) = left (pct $ (fromIntegral x)*oneSixth + 5) >> top (pct $ (fromIntegral y)*oneQuarter+5)

placeholderTileCss :: (Int, Int) -> Css
placeholderTileCss position = positionCss position >> commonBuildingCss

highlightedPlaceholderTileCss :: Css
highlightedPlaceholderTileCss = do
  backgroundColor "#FF4444" >> opacity 0.7
  width (pct 100)
  height (pct 100)

highlightedValidPlaceholderTileCss :: Css
highlightedValidPlaceholderTileCss = do
  backgroundColor "#44FF44" >> opacity 0.4
  width (pct 100)
  height (pct 100)

hiddenPlaceholderTileCss :: Css
hiddenPlaceholderTileCss = display block
