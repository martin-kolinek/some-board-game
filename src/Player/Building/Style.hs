{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Player.Building.Style where

import Common.CssClass
import Common.CommonClasses
import Player.Building.Types

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

hiddenPotentialCropClass :: CssClass
hiddenPotentialCropClass = CssClass "hidden-potential-crop"
validPotentialCropClass :: CssClass
validPotentialCropClass = CssClass "valid-potential-crop"
invalidPotentialCropClass :: CssClass
invalidPotentialCropClass = CssClass "invalid-potential-crop"

hiddenPotentialBarnClass :: CssClass
hiddenPotentialBarnClass = CssClass "hidden-potential-barn"
validPotentialBarnClass :: CssClass
validPotentialBarnClass = CssClass "valid-potential-barn"
invalidPotentialBarnClass :: CssClass
invalidPotentialBarnClass = CssClass "invalid-potential-barn"

animalClass :: CssClass
animalClass = CssClass "animal-occupant"
highlightedAnimalClass :: CssClass
highlightedAnimalClass = CssClass "highlighted-animal-occupant"

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
  star # classSelector hiddenPotentialCropClass ? display none
  star # classSelector validPotentialCropClass ? do
    color green
    fontWeight bold
  star # classSelector invalidPotentialCropClass ? do
    color red
    fontWeight bold
  star # classSelector hiddenPotentialBarnClass ? display none
  star # classSelector validPotentialBarnClass ? do
    color blue
    fontWeight bold
  star # classSelector invalidPotentialBarnClass ? do
    color red
    fontWeight bold
  star # classSelector highlightedAnimalClass ? do
    fontWeight bold
  star # classSelector animalClass ? do
    opacity 1
    cursor pointer
    width (em 3.7)
    margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)
    transitions [("width", sec 0.5, easeInOut, sec 0), ("margin", sec 0.5, easeInOut, sec 0), ("opacity", sec 0.5, easeInOut, sec 0.5)]
    display inlineBlock
    height (em 3.7)
    backgroundSize contain
    backgroundRepeat noRepeat
    animationName "animal-kf"
    animationDuration (sec 1)
    animationIterationCount (iterationCount 1)
  star # classSelector animalClass # classSelector fadeClass ? do
    opacity 0
    width (px 0)
    margin (px 0) (px 0) (px 0) (px 0)
    transitions [("opacity", sec 0.5, easeInOut, sec 0), ("width", sec 0.5, easeInOut, sec 0.5), ("margin", sec 0.5, easeInOut, sec 0.5)]
  keyframes "animal-kf" [
      (0, opacity 0),
      (100, opacity 1)]

oneSixth :: Double
oneSixth = 15
oneQuarter :: Double
oneQuarter = 20

smallBuildingTypeCss :: SmallBuildingType -> Css
smallBuildingTypeCss Grass = background (url "data/grass.svg")
smallBuildingTypeCss Forest = background (url "data/forest.svg")
smallBuildingTypeCss Rock = background (url "data/rock.svg")
smallBuildingTypeCss InitialRoom = background (url "data/init_room.svg")
smallBuildingTypeCss Cave = background (url "data/cave.svg")
smallBuildingTypeCss Passage = background (url "data/passage.svg")
smallBuildingTypeCss LivingRoom = background (url "data/living_room.svg")
smallBuildingTypeCss Field = background (url "data/field.svg")
smallBuildingTypeCss SmallPasture = background (url "data/small_pasture.svg")

largeBuildingTypeCss :: LargeBuildingType -> Css
largeBuildingTypeCss LargePasture = background (url "data/large_pasture.svg")

buildingTileCss :: TileBuildingType -> Css
buildingTileCss (SingleTileBuilding buildingType) = smallBuildingTypeCss buildingType
buildingTileCss (BuildingPart buildingType direction) = largeBuildingTypeCss buildingType >> rotateCss direction

buildingCss :: Rules.Position -> TileBuildingType -> Css
buildingCss position tileBuildingType = buildingTileCss tileBuildingType >> positionCss position >> commonBuildingCss

commonBuildingCss :: Css
commonBuildingCss = width (pct oneSixth) >> height (pct oneQuarter) >> position absolute >> backgroundSize cover

buildingSelectionCss :: Maybe TileBuildingType -> Css
buildingSelectionCss Nothing = display none
buildingSelectionCss (Just tileBuildingType) = do
  buildingTileCss tileBuildingType
  width (em 4)
  height (em 4)
  backgroundSize cover
  display inlineBlock
  verticalAlign middle
  lineHeight (em 4)

positionCss :: (Int, Int) -> Css
positionCss (x, y) = left (pct $ (fromIntegral x)*oneSixth + 5) >> top (pct $ (fromIntegral y)*oneQuarter+5)

rotateCss :: Rules.Direction -> Css
rotateCss dir =
  let degs = case dir of
        DirectionUp -> 0
        DirectionLeft -> 90
        DirectionDown -> 180
        DirectionRight -> 270
  in transform $ rotate $ deg degs

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
