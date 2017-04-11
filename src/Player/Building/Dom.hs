{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo, TupleSections, OverloadedStrings #-}

module Player.Building.Dom where

import Rules
import Common.DomUtil
import Player.Building.Style
import Player.Types
import Player.Worker.Dom

import Reflex.Dom
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Either
import Data.AdditiveGroup
import Prelude hiding (error)
import Data.Foldable (fold)
import qualified Data.Text as T
import Data.Align
import Data.These

data PotentialBuilding = ValidBuilding BuildingType | InvalidBuilding BuildingType | NoBuilding deriving (Eq, Show)

data TileInfo = TileInfo
  {
    tileBuilding :: BuildingType,
    tileOccupants :: [BuildingOccupant],
    tileOccupantErrors :: [String],
    tilePotentialBuildings :: PotentialBuilding,
    tileCrops :: [CropType]
  } deriving Show

createTiles :: Universe -> PlayerId -> Maybe Position -> Direction -> [BuildingType] -> PlantingStatus -> M.Map Position TileInfo
createTiles universe playerId hoveredPositionMaybe hoveredDirection currentBuildings plantingStatus =
  let buildings = getBuildingSpace universe playerId
      getBuiltBuildingIndex position = if Just position == hoveredPositionMaybe then 0 else if Just (position ^-^ directionAddition hoveredDirection) == hoveredPositionMaybe then 1 else 2
      getPotentialBuildingType position = listToMaybe $ drop (getBuiltBuildingIndex position) currentBuildings
      getPotentialBuilding position = case (getPotentialBuildingType position, hoveredPositionMaybe) of
        (Just bt, Just hoveredPosition) -> if isLeft (buildBuildings playerId hoveredPosition hoveredDirection currentBuildings universe) then InvalidBuilding bt else ValidBuilding bt
        _ -> NoBuilding
      getTileOccupants position = findVisibleOccupants universe playerId position
      getTileOccupantErrors position = fst <$> (filter ((== position) . snd) $ getOccupantErrors universe playerId)
      getCrops position = case plantingStatus of
        IsNotPlanting -> []
        IsPlanting crops _ -> [cropType | (cropType, pos) <- crops, pos == position]
      getBuildingTile (Building buildingType pos) = (pos, TileInfo buildingType (getTileOccupants pos) (getTileOccupantErrors pos) (getPotentialBuilding pos) (getCrops pos))
  in M.fromList $ getBuildingTile <$> buildings

findVisibleOccupants :: Universe -> PlayerId -> Position -> [BuildingOccupant]
findVisibleOccupants universe playerId position = filter isOccupantVisible $ M.findWithDefault [] position $ getBuildingOccupants universe playerId
  where isOccupantVisible (WorkerOccupant workerId) = isNothing $ getWorkerWorkplace universe workerId
        isOccupantVisible _ = True

data TileResults t = TileResults
  {
    clickedPositions :: Event t [Position],
    clickedOccupants :: Event t [BuildingOccupant],
    hoveredPositions :: Dynamic t [Position]
  }

extractTileResultsFromDynamic :: MonadWidget t m => Dynamic t (TileResults t) -> m (TileResults t)
extractTileResultsFromDynamic resultsDyn = do
  let posEvents = switchPromptlyDyn $ clickedPositions <$> resultsDyn
      occupantEvents = switchPromptlyDyn $ clickedOccupants <$> resultsDyn
  hoveredPos <- holdDyn [] $ switch (current $ updated <$> hoveredPositions <$> resultsDyn)
  return $ TileResults posEvents occupantEvents hoveredPos

instance Reflex t => Monoid (TileResults t) where
  mempty = TileResults mempty mempty mempty
  mappend (TileResults a1 b1 c1) (TileResults a2 b2 c2) = TileResults (a1 <> a2) (b1 <> b2) (c1 <> c2)

drawTileInfo :: PlayerWidget t m => Dynamic t (Maybe BuildingOccupant) -> Position -> Dynamic t TileInfo -> m (TileResults t)
drawTileInfo selectedOccupantDyn position tileInfoDyn = do
  let tilePotentialBuildingsDyn = uniqDyn $ tilePotentialBuildings <$> tileInfoDyn
      tileOccupantErrorsDyn = uniqDyn $ tileOccupantErrors <$> tileInfoDyn
      tileOccupantsDyn = uniqDyn $ tileOccupants <$> tileInfoDyn
      tileBuildingDyn = uniqDyn $ tileBuilding <$> tileInfoDyn
      tileCropsDyn = uniqDyn $ tileCrops <$> tileInfoDyn
      getBuildingToDraw tilePotBuild tileBuild = case tilePotBuild of
        NoBuilding -> tileBuild
        ValidBuilding b -> b
        InvalidBuilding b -> b
      getOverlayCss tilePotBuild = case tilePotBuild of
        NoBuilding -> hiddenPlaceholderTileCss
        ValidBuilding _ -> highlightedValidPlaceholderTileCss
        InvalidBuilding _ -> highlightedPlaceholderTileCss
      cropText [] = ""
      cropText x = T.pack $ show x
  (divEl, inner) <- divAttributeLikeDyn' (flip buildingCss position <$> (getBuildingToDraw <$> tilePotentialBuildingsDyn <*> tileBuildingDyn)) $ do
    divAttributeLikeDyn (getOverlayCss <$> tilePotentialBuildingsDyn) $ do
      drawOccupantErrors $ tileOccupantErrorsDyn
      result <- divAttributeLike occupantContainerClass $ do
        let combineOccupantClicks workers = M.elems <$> mergeMap workers
        occupantClicks <- animatedList (fromRational 1) (tileOccupantsDyn) (drawWorkplaceOccupant selectedOccupantDyn)
        let combinedClicks = combineOccupantClicks <$> occupantClicks
        return $ switch (current combinedClicks)
      dynText $ cropText <$> tileCropsDyn
      return result
  hoveredPos <- holdDyn [] (leftmost [const [] <$> domEvent Mouseleave divEl, const [position] <$> domEvent Mouseenter divEl])
  return $ TileResults (const [position] <$> domEvent Click divEl) inner hoveredPos

drawBuildings :: PlayerWidget t m => Dynamic t (Maybe BuildingOccupant) -> Dynamic t BuildingStatus -> Dynamic t PlantingStatus -> m (Event t Position, Event t BuildingOccupant)
drawBuildings selectedOccupantDyn buildingStatusDyn plantingStatusDyn= do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let getDirection (IsBuilding _ dir) = dir
      getDirection _ = DirectionDown
      getBuildings (IsBuilding b _) = b
      getBuildings _ = []
      directionDyn = uniqDyn $ getDirection <$> buildingStatusDyn
      selectedBuildingDyn = uniqDyn $ getBuildings <$> buildingStatusDyn
  rec
    let tiles = createTiles <$> universeDyn <*> pure playerId <*> (listToMaybe <$> hoveredPositionDyn) <*> directionDyn <*> selectedBuildingDyn <*> plantingStatusDyn
    result <- listWithKey tiles (drawTileInfo selectedOccupantDyn)
    (TileResults clickedPos clickedOcc hoveredPositionDyn) <- extractTileResultsFromDynamic $ fold <$> result
  return (fmapMaybe listToMaybe clickedPos, fmapMaybe listToMaybe clickedOcc)

data PlantingStatus = IsPlanting [CropToPlant] (Maybe CropType) | IsNotPlanting deriving Eq
data BuildingStatus = IsBuilding [BuildingType] Direction | IsNotBuilding deriving Eq

drawBuildingSelection :: PlayerWidget t m => Dynamic t PlantingStatus -> m (Dynamic t BuildingStatus)
drawBuildingSelection plantingStatusDyn = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let drawSelection :: PlayerWidget t2 m2 => Bool -> m2 (Dynamic t2 BuildingStatus)
      drawSelection True = do
        universeDyn2 <- askUniverseDyn
        playerId2 <- askPlayerId
        let currentBuildingDyn = (uniqDyn $ currentlyBuiltBuildings <$> universeDyn2 <*> pure playerId2)
        directionDyn <- drawRotationButton
        behaviorChanges <- dyn $ drawSelectionForPossibilities <$> currentBuildingDyn
        nestedBehavior <- holdDyn (pure []) behaviorChanges
        return $ IsBuilding <$> join nestedBehavior <*> directionDyn
      drawSelection False = do
        return $ constDyn IsNotBuilding
      canCurrentlyBuildDyn = not . null <$> (currentlyBuiltBuildings <$> universeDyn <*> pure playerId)
      stopBuildingEv = ffilter null $ updated (currentlyBuiltBuildings <$> universeDyn <*> pure playerId)
      isNotPlantingDyn = (== IsNotPlanting) <$> plantingStatusDyn
      canBuildDyn = (&&) <$> canCurrentlyBuildDyn <*> isNotPlantingDyn
      drawBuildButton :: PlayerWidget t2 m2 => Dynamic t2 Bool -> Event t2 a -> m2 (Dynamic t2 Bool)
      drawBuildButton visible stopBuildingEvent = do
        let txt True = "Cancel"
            txt False = "Build"
            cls True = buildButtonClass
            cls False = hiddenButtonClass
        rec
          (divEl, _) <- divAttributeLikeDyn' (cls <$> visible) $ dynText (txt <$> isBuilding)
          isBuilding <- toggleWithReset False (domEvent Click divEl) stopBuildingEvent
        return isBuilding
  isBuildingDyn <- drawBuildButton canBuildDyn stopBuildingEv
  let isBuildingAndCanBuild = (&&) <$> isBuildingDyn <*> canCurrentlyBuildDyn
  join <$> (holdDyn (constDyn IsNotBuilding) =<< (dyn $ drawSelection <$> isBuildingAndCanBuild))

createBuildActions :: Reflex t => Dynamic t BuildingStatus -> Event t Position -> Event t PlayerAction
createBuildActions buildingStatusDyn positionEvent =
  let createBuildAction (IsBuilding buildings direction) position = Just $ \plId -> buildBuildings plId position direction buildings
      createBuildAction _ _ = Nothing
  in attachWithMaybe createBuildAction (current buildingStatusDyn) positionEvent

drawPlanting :: PlayerWidget t m => Event t Position -> Dynamic t BuildingStatus -> m (Dynamic t PlantingStatus)
drawPlanting clickedPositionsEvent buildingStatusDyn = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let drawPlanting' :: PlayerWidget t2 m2 => Event t2 Position -> Bool -> m2 (Dynamic t2 PlantingStatus)
      drawPlanting' _ False = return $ constDyn IsNotPlanting
      drawPlanting' clickedPos True = do
        selectedCrop <- drawCropSelection
        plantedCropsDyn <- createPlantedCrops selectedCrop clickedPos
        return $ IsPlanting <$> plantedCropsDyn <*> selectedCrop
      canPlantDyn = (&&) <$> (isPlantingCrops <$> universeDyn <*> pure playerId) <*> ((== IsNotBuilding) <$> buildingStatusDyn)
      stopPlantingEv = ffilter (==False) $ updated (isPlantingCrops <$> universeDyn <*> pure playerId)
      drawPlantingButton :: PlayerWidget t2 m2 => Dynamic t2 Bool -> Event t2 a -> m2 (Dynamic t2 Bool)
      drawPlantingButton visible stopPlantingEvent = do
        let txt True = "Cancel"
            txt False = "Plant"
            cls True = plantCropsButtonClass
            cls False = hiddenButtonClass
        rec
          (divEl, _) <- divAttributeLikeDyn' (cls <$> visible) $ dynText $ txt <$> isPlanting
          isPlanting <- toggleWithReset False (domEvent Click divEl) stopPlantingEvent
        return $ isPlanting
  isPlanting <- drawPlantingButton canPlantDyn stopPlantingEv
  let isPlantingAndCanPlant = (&&) <$> isPlanting <*> canPlantDyn
  fmap join $ holdDyn (constDyn IsNotPlanting) =<< (dyn $ drawPlanting' clickedPositionsEvent <$> isPlantingAndCanPlant)

toggleWithReset :: MonadWidget t m => Bool -> Event t tog -> Event t reset -> m (Dynamic t Bool)
toggleWithReset initialValue toggleEvent resetEvent = do
  let combine (This _) cur = not cur
      combine _ _ = False
  foldDyn combine initialValue (align toggleEvent resetEvent)

drawCropSelection :: PlayerWidget t m => m (Dynamic t (Maybe CropType))
drawCropSelection = do
  let cls neededCropType actualCropType = cropTypeClass <> if Just neededCropType == actualCropType then highlightedCropTypeClass else mempty
  rec
    (potatoElement, _) <- divAttributeLikeDyn' (cls Potatoes <$> selectedCropType) $ text "Potatoes"
    (wheatElement, _) <- divAttributeLikeDyn' (cls Wheat <$> selectedCropType) $ text "Wheat"
    let potatoClicks = const Potatoes <$> domEvent Click potatoElement
        wheatClicks = const Wheat <$> domEvent Click wheatElement
    selectedCropType <- foldDyn (\next curr -> if curr == Just next then Nothing else Just next) Nothing (leftmost [potatoClicks, wheatClicks])
  return selectedCropType

createPlantedCrops :: PlayerWidget t m => Dynamic t (Maybe CropType) -> Event t Position -> m (Dynamic t [CropToPlant])
createPlantedCrops selectedCrop positionClicks = do
  let createCropToPlant (Just cropType) pos = Just (cropType, pos)
      createCropToPlant _ _ = Nothing
      cropToPlantEvent = attachWithMaybe createCropToPlant (current selectedCrop) positionClicks
  foldDyn (:) [] cropToPlantEvent

drawPlantingConfirmation :: PlayerWidget t m => Dynamic t PlantingStatus -> m (Event t PlayerAction)
drawPlantingConfirmation cropsToPlantDyn = do
  let createAction (IsPlanting crops _) = Just $ \plId -> plantCrops plId crops
      createAction _ = Nothing
      cls IsNotPlanting = hiddenButtonClass
      cls _ = plantCropsButtonClass
  (confirmEl, _) <- divAttributeLikeDyn' (cls <$> cropsToPlantDyn) $ text "Confirm"
  return $ fmapMaybe id $ createAction <$> tag (current cropsToPlantDyn) (domEvent Click confirmEl)

drawBuildingSpace :: PlayerWidget t m => m (PlayerExports t)
drawBuildingSpace = divAttributeLike buildingSpaceClass $ do
  rec
    (clickedPosition, clickedOccupant) <- drawBuildings selectedOccupantDyn buildingStatusDyn plantingStatusDyn
    selectedOccupantDyn <- createSelectedOccupant clickedOccupant
    (plantingStatusDyn, buildingStatusDyn, plantActions) <- divAttributeLike buildingOptionsClass $ do
      buildingStatusDynInner <- drawBuildingSelection plantingStatusDyn
      plantingStatusDynInner <- drawPlanting clickedPosition buildingStatusDyn
      plantActs <- drawPlantingConfirmation plantingStatusDyn
      return (plantingStatusDynInner, buildingStatusDynInner, plantActs)
    let buildingActions = createBuildActions buildingStatusDyn clickedPosition
        selectedWorkerDyn = (workerFromOccupant =<<) <$> selectedOccupantDyn
        allActions = leftmost [buildingActions, plantActions]
  return $ PlayerExports selectedWorkerDyn allActions

drawOccupantErrors :: (MonadWidget t m) => Dynamic t [String] -> m ()
drawOccupantErrors errors =
  void $ simpleList errors $ \errorDyn ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (dynText $ T.pack <$> errorDyn)

drawRotationButton :: PlayerWidget t m => m (Dynamic t Direction)
drawRotationButton = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let currentBuildingDyn = (uniqDyn $ currentlyBuiltBuildings <$> universeDyn <*> pure playerId)
      getClass [] = hiddenButtonClass
      getClass _ = rotateButtonClass
  (rotateElement, _) <- divAttributeLikeDyn' (getClass <$> currentBuildingDyn) $ return ()
  let rotateClicks = domEvent Click rotateElement
  foldDyn (const nextDirection) DirectionDown rotateClicks

drawSelectionForPossibilities :: MonadWidget t m => [[BuildingType]] -> m (Dynamic t [BuildingType])
drawSelectionForPossibilities [] = return $ pure []
drawSelectionForPossibilities possibilities = do
  let possibilitiesCount = length possibilities
  rec
    (leftEl, _) <- divAttributeLike' switchBuildingLeftClass $ return ()
    void $ simpleList currentBuilding $ \buildingTypeDyn -> divAttributeLikeDyn (buildingSelectionCss <$> buildingTypeDyn) $ return ()
    (rightEl, _) <- divAttributeLike' switchBuildingRightClass $ return ()
    let switchEvent = leftmost [const (-1 :: Int) <$> domEvent Click leftEl, const (1 :: Int) <$> domEvent Click rightEl]
    currentIndex <- foldDyn (\x y -> mod (x + y) possibilitiesCount) (0 :: Int) switchEvent
    let currentBuilding = (possibilities !!) <$> currentIndex
  return $ currentBuilding

createSelectedOccupant :: PlayerWidget t m => Event t BuildingOccupant -> m (Dynamic t (Maybe BuildingOccupant))
createSelectedOccupant occupants = do
  let removeInvalidOccupants maybeOccupant universe = do
          occupant <- maybeOccupant
          guard $ isOccupantValid occupant universe
          return occupant
      justOccupants = Just <$> occupants
  heldOccupants <- hold Nothing justOccupants
  universeChangeEvents <- updated <$> askUniverseDyn
  let filteredSelection = attachWith removeInvalidOccupants heldOccupants universeChangeEvents
  selectedOccupants <- holdDyn Nothing $ leftmost [filteredSelection, justOccupants]
  return (uniqDyn selectedOccupants)

isOccupantValid :: BuildingOccupant -> Universe -> Bool
isOccupantValid (WorkerOccupant workerId) universe = isNothing (getWorkerWorkplace universe workerId)
isOccupantValid _ _ = True

drawWorkplaceOccupant :: PlayerWidget t m =>
  Dynamic t (Maybe BuildingOccupant) -> BuildingOccupant -> Dynamic t AnimationState -> m (Event t BuildingOccupant)
drawWorkplaceOccupant selectedOccupant (WorkerOccupant workerId) animationState = do
  let selectedWorker = (workerFromOccupant =<<) <$> selectedOccupant
  workerEvent <- drawWorker selectedWorker workerId animationState
  return $ WorkerOccupant <$> workerEvent
drawWorkplaceOccupant _ _ _ = return never

workerFromOccupant :: BuildingOccupant -> Maybe WorkerId
workerFromOccupant (WorkerOccupant workerId) = Just workerId
workerFromOccupant _ = Nothing

nextDirection :: Direction -> Direction
nextDirection DirectionUp = DirectionRight
nextDirection DirectionRight = DirectionDown
nextDirection DirectionDown = DirectionLeft
nextDirection DirectionLeft = DirectionUp
