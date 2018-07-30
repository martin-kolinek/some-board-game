{-# LANGUAGE TypeFamilies #-}
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
import Player.Building.Types
import Types

import Reflex.Dom hiding (crop)
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
import Data.Default
import Data.List (find)
import qualified Data.List.NonEmpty as NE

data PotentialBuilding = ValidBuilding TileBuildingType | InvalidBuilding TileBuildingType | NoBuilding deriving (Eq, Show)
data PotentialCrop = ValidCrop CropType | InvalidCrop CropType | NoCrop deriving (Eq, Show)
data PotentialBarn = ValidBarn | InvalidBarn | NoBarn deriving (Eq, Show)

data TileInfo = TileInfo
  {
    tileBuilding :: TileBuildingType,
    tileOccupants :: [BuildingOccupant],
    tileOccupantErrors :: [String],
    tileCrops :: [CropType],
    tileHasBarn :: Bool
  } deriving Show

data DynamicInfo t = DynamicInfo {
    currentlyHoveredPosition :: Dynamic t (Maybe Position),
    currentSelectedOccupant :: Dynamic t (Maybe BuildingOccupant),
    currentPlantingStatus :: Dynamic t PlantingStatus,
    currentlyBuilding :: Dynamic t BuildingStatus,
    currentlyBuildingBarn :: Dynamic t BarnBuildingStatus
  }

getPotentialBuilding :: BuildingStatus -> Maybe Position -> Position -> PlayerId -> Universe -> PotentialBuilding
getPotentialBuilding (IsBuilding buildingDescription direction) (Just hoveredPosition) position playerId universe =
  case potentialBuildingType of
        Just bt -> if isLeft (buildBuildings playerId hoveredPosition direction buildingDescription universe) then InvalidBuilding bt else ValidBuilding bt
        _ -> NoBuilding
  where tileOffset = if position == hoveredPosition then ExactPosition else if position ^-^ directionAddition direction == hoveredPosition then NextPosition else OtherPosition
        potentialBuildingType = getTileBuildingType direction tileOffset buildingDescription
getPotentialBuilding _ _ _ _ _ = NoBuilding

createTiles :: Universe -> PlayerId -> M.Map Position TileInfo
createTiles universe playerId =
  let buildings = getBuildingSpace universe playerId
      getTileOccupants position = findVisibleOccupants universe playerId position
      getTileOccupantErrors position = fst <$> (filter ((== position) . snd) $ getOccupantErrors universe playerId)
      getUniverseCrops position = join $ maybeToList $ do
        (PlantedCrop cropType cnt) <- M.lookup position (getPlantedCrops universe playerId)
        return $ replicate cnt cropType
      getHasBarn = (`elem` getBarns universe playerId)
      makeTile tileBuildingType pos = (pos, TileInfo
          (tileBuildingType)
          (getTileOccupants pos)
          (getTileOccupantErrors pos)
          (getUniverseCrops pos)
          (getHasBarn pos))
      getBuildingTiles (SmallBuilding buildingType pos) = [makeTile (SingleTileBuilding buildingType) pos]
      getBuildingTiles (LargeBuilding buildingType pos dir) =
        [makeTile (BuildingPart buildingType dir) pos, makeTile (BuildingPart buildingType (oppositeDirection dir)) pos]
    in M.fromList $ getBuildingTiles =<< buildings

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

getPotentialCrop :: Universe -> PlayerId -> PlantingStatus -> Position -> Bool -> PotentialCrop
getPotentialCrop universe plId (IsPlanting crops selectedCrop) position hovered =
  let existing = do
        (crop, _) <- find ((== position) . snd) crops
        return $ ValidCrop crop
      new = do
        selCrop <- selectedCrop
        guard $ hovered
        return $ if isLeft $ plantCrops plId ((selCrop, position) : crops) universe
          then ValidCrop selCrop
          else InvalidCrop selCrop
  in fromMaybe NoCrop $ listToMaybe $ catMaybes [existing, new]
getPotentialCrop _ _ _ _ _ = NoCrop

getPotentialBarn :: BarnBuildingStatus -> Position -> Bool -> PlayerId -> Universe -> PotentialBarn
getPotentialBarn IsBuildingBarn position True playerId universe = if isLeft $ buildBarn playerId position universe
                                                                  then InvalidBarn
                                                                  else ValidBarn
getPotentialBarn _ _ _ _ _ = NoBarn

drawTileInfo :: PlayerWidget t m x => DynamicInfo t -> Position -> TileInfo -> m (TileResults t)
drawTileInfo dynamicInfo position tileInfo = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  isHovered <- holdUniqDyn (((==) . Just) <$> pure position <*> currentlyHoveredPosition dynamicInfo)
  let potentialBuildingDyn = getPotentialBuilding <$> (currentlyBuilding dynamicInfo) <*> (currentlyHoveredPosition dynamicInfo) <*> pure position <*> pure playerId <*> universeDyn
      getBuildingToDraw tilePotBuild tileBuild = case tilePotBuild of
        NoBuilding -> tileBuild
        ValidBuilding b -> b
        InvalidBuilding b -> b
      getOverlayCss tilePotBuild = case tilePotBuild of
        NoBuilding -> hiddenPlaceholderTileCss
        ValidBuilding _ -> highlightedValidPlaceholderTileCss
        InvalidBuilding _ -> highlightedPlaceholderTileCss
      getPotentialCropClass NoCrop = hiddenPotentialCropClass
      getPotentialCropClass (ValidCrop _) = validPotentialCropClass
      getPotentialCropClass (InvalidCrop _) = invalidPotentialCropClass
      getPotentialCropText NoCrop = ""
      getPotentialCropText (ValidCrop tp) = T.pack $ show tp
      getPotentialCropText (InvalidCrop tp) = T.pack $ show tp
      getPotentialBarnClass NoBarn = hiddenPotentialBarnClass
      getPotentialBarnClass ValidBarn = validPotentialBarnClass
      getPotentialBarnClass InvalidBarn = invalidPotentialBarnClass
      cropText [] = ""
      cropText x = T.pack $ show x
      drawBarn True = text "Barn"
      drawBarn False = return ()
      tilePotentialCropDyn = getPotentialCrop <$> universeDyn <*> pure playerId <*> currentPlantingStatus dynamicInfo <*> pure position <*> isHovered
      tilePotentialBarnDyn = getPotentialBarn <$> currentlyBuildingBarn dynamicInfo <*> pure position <*> isHovered <*> pure playerId <*> universeDyn
  (divEl, inner) <- divAttributeLikeDyn' (buildingCss position <$> (getBuildingToDraw <$> potentialBuildingDyn <*> pure (tileBuilding tileInfo))) $ do
    divAttributeLikeDyn (getOverlayCss <$> potentialBuildingDyn) $ do
      drawOccupantErrors $ tileOccupantErrors tileInfo
      result <- divAttributeLike occupantContainerClass $ do
        occupantClicks <- forM (tileOccupants tileInfo) (drawWorkplaceOccupant $ currentSelectedOccupant dynamicInfo)
        let combinedClicks = NE.toList <$> mergeList occupantClicks
        return combinedClicks
      text $ cropText $ tileCrops tileInfo
      drawBarn $ tileHasBarn tileInfo
      divAttributeLikeDyn (getPotentialCropClass <$> tilePotentialCropDyn) $ dynText (getPotentialCropText <$> tilePotentialCropDyn)
      divAttributeLikeDyn (getPotentialBarnClass <$> tilePotentialBarnDyn) $ drawBarn True
      return result
  hoveredPos <- holdDyn [] (leftmost [const [] <$> domEvent Mouseleave divEl, const [position] <$> domEvent Mouseenter divEl])
  return $ TileResults (const [position] <$> domEvent Click divEl) inner hoveredPos

drawBuildings :: PlayerWidget t m x => DynamicInfo t -> m (Event t Position, Event t BuildingOccupant, Dynamic t (Maybe Position))
drawBuildings dynamicInfo = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  rec
    let tiles = createTiles <$> universeDyn <*> pure playerId
    result <- listWithKeyNonDyn tiles (drawTileInfo dynamicInfo)
    (TileResults clickedPos clickedOcc hoveredPositionDyn) <- extractTileResultsFromDynamic $ fold <$> result
  return (fmapMaybe listToMaybe clickedPos, fmapMaybe listToMaybe clickedOcc, listToMaybe <$> hoveredPositionDyn)

data PlantingStatus = IsPlanting [CropToPlant] (Maybe CropType) | IsNotPlanting deriving (Eq, Show)
instance Default PlantingStatus where
  def = IsNotPlanting
data BuildingStatus = IsBuilding BuildingDescription Direction | IsNotBuilding deriving (Eq, Show)
instance Default BuildingStatus where
  def = IsNotBuilding
data BarnBuildingStatus = IsBuildingBarn | IsNotBuildingBarn deriving (Eq, Show)
instance Default BarnBuildingStatus where
  def = IsNotBuildingBarn

toggleButton :: (PlayerWidget t m x, Default a) => T.Text -> Dynamic t Bool -> (Universe -> PlayerId -> Bool) -> m (Dynamic t a) -> m (Dynamic t a)
toggleButton name noOtherActivityDyn canDoActivityFunc inner = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  canDoActivityDyn <- holdUniqDyn $ canDoActivityFunc <$> universeDyn <*> pure playerId
  isActivityPossible <- holdUniqDyn $ (&&) <$> canDoActivityDyn <*> noOtherActivityDyn
  let stopActivity = ffilter (== False) $ updated canDoActivityDyn
      drawButton :: PlayerWidget t2 m2 x2 => Dynamic t2 Bool -> Event t2 a -> m2 (Dynamic t2 Bool)
      drawButton visible stopEvent = do
        let txt True = "Cancel"
            txt False = name
            cls True = buildButtonClass
            cls False = hiddenButtonClass
        rec
          (divEl, _) <- divAttributeLikeDyn' (cls <$> visible) $ dynText (txt <$> isBuilding)
          isBuilding <- toggleWithReset False (domEvent Click divEl) stopEvent
        return isBuilding
      drawInner True = inner
      drawInner False = return $ constDyn def
  isDoingActivity <- drawButton isActivityPossible stopActivity
  let isDoingActivityAndCan= (&&) <$> isDoingActivity <*> isActivityPossible
  join <$> (holdDyn (constDyn def) =<< (dyn $ drawInner <$> isDoingActivityAndCan))

drawBuildingSelection :: PlayerWidget t m x => Dynamic t Bool -> m (Dynamic t BuildingStatus)
drawBuildingSelection canBuildDyn = toggleButton "Build" canBuildDyn (\u p -> not $ null $ currentlyBuiltBuildings u p) $ do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  currentBuildingDyn <- holdUniqDyn $ currentlyBuiltBuildings <$> universeDyn <*> pure playerId
  directionDyn <- drawRotationButton
  behaviorChanges <- dyn $ drawSelectionForPossibilities <$> currentBuildingDyn
  nestedBehavior <- holdDyn (pure Nothing) behaviorChanges
  let makeBuildingStatus (Just buildingDescription) dir = IsBuilding buildingDescription dir
      makeBuildingStatus Nothing _ = IsNotBuilding
  return $ makeBuildingStatus <$> join nestedBehavior <*> directionDyn

createBuildActions :: Reflex t => Dynamic t BuildingStatus -> Event t Position -> Event t PlayerAction
createBuildActions buildingStatusDyn positionEvent =
  let createBuildAction (IsBuilding buildings direction) position = Just $ \plId -> buildBuildings plId position direction buildings
      createBuildAction _ _ = Nothing
  in attachWithMaybe createBuildAction (current buildingStatusDyn) positionEvent

drawPlanting :: PlayerWidget t m x => Event t Position -> Dynamic t Bool -> m (Dynamic t PlantingStatus)
drawPlanting clickedPositionsEvent canPlantDyn = toggleButton "Plant" canPlantDyn isPlantingCrops $ do
  selectedCrop <- drawCropSelection
  plantedCropsDyn <- createPlantedCrops selectedCrop clickedPositionsEvent
  return $ IsPlanting <$> plantedCropsDyn <*> selectedCrop

drawBarnBuilding :: PlayerWidget t m x => Dynamic t Bool -> m (Dynamic t BarnBuildingStatus)
drawBarnBuilding canBuildBarnDyn = toggleButton "Build Barn" canBuildBarnDyn canBuildBarn $ do
  return (constDyn IsBuildingBarn)

toggleWithReset :: MonadWidget t m => Bool -> Event t tog -> Event t reset -> m (Dynamic t Bool)
toggleWithReset initialValue toggleEvent resetEvent = do
  let combine (This _) cur = not cur
      combine _ _ = False
  foldDyn combine initialValue (align toggleEvent resetEvent)

drawCropSelection :: PlayerWidget t m x => m (Dynamic t (Maybe CropType))
drawCropSelection = do
  let cls _ _ False = hiddenButtonClass
      cls neededCropType actualCropType _ = cropTypeClass <> if Just neededCropType == actualCropType then highlightedCropTypeClass else mempty
      cropFunc Potatoes = getPotatoAmount
      cropFunc Wheat = getWheatAmount
      hasEnoughResources cropType universe playerId = cropFunc cropType (getPlayerResources universe playerId) > 0
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  rec
    (potatoElement, _) <- divAttributeLikeDyn' (cls Potatoes <$> selectedCropType <*> (hasEnoughResources Potatoes <$> universeDyn <*> pure playerId)) $ text "Potatoes"
    (wheatElement, _) <- divAttributeLikeDyn' (cls Wheat <$> selectedCropType <*> (hasEnoughResources Wheat <$> universeDyn <*> pure playerId)) $ text "Wheat"
    let potatoClicks = const Potatoes <$> domEvent Click potatoElement
        wheatClicks = const Wheat <$> domEvent Click wheatElement
    selectedCropType <- foldDyn (\next curr -> if curr == Just next then Nothing else Just next) Nothing (leftmost [potatoClicks, wheatClicks])
  return selectedCropType

createPlantedCrops :: PlayerWidget t m x => Dynamic t (Maybe CropType) -> Event t Position -> m (Dynamic t [CropToPlant])
createPlantedCrops selectedCrop positionClicks = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let createCropToPlant (Just cropType) pos = Just (cropType, pos)
      createCropToPlant _ _ = Nothing
      cropToPlantEvent = attachWithMaybe createCropToPlant (current selectedCrop) positionClicks
      combine crop existingCrops = do
        u <- sample (current universeDyn)
        let combined = crop : existingCrops
        if isLeft (plantCrops playerId combined u)
          then return Nothing else return $ Just combined
  foldDynMaybeM combine [] cropToPlantEvent

drawPlantingConfirmation :: PlayerWidget t m x => Dynamic t PlantingStatus -> m (Event t PlayerAction)
drawPlantingConfirmation cropsToPlantDyn = do
  let createAction (IsPlanting crops _) = Just $ \plId -> plantCrops plId crops
      createAction _ = Nothing
      cls IsNotPlanting = hiddenButtonClass
      cls _ = plantCropsButtonClass
  (confirmEl, _) <- divAttributeLikeDyn' (cls <$> cropsToPlantDyn) $ text "Confirm"
  return $ fmapMaybe id $ createAction <$> tag (current cropsToPlantDyn) (domEvent Click confirmEl)

isBuildingPossible :: Applicative f => f PlantingStatus -> f BarnBuildingStatus -> f Bool
isBuildingPossible planting barnBuilding = (&&) <$> ((== IsNotPlanting) <$> planting) <*> ((== IsNotBuildingBarn) <$> barnBuilding)
isPlantingPossible :: Applicative f => f BuildingStatus -> f BarnBuildingStatus -> f Bool
isPlantingPossible building barnBuilding = (&&) <$> ((== IsNotBuilding) <$> building) <*> ((== IsNotBuildingBarn) <$> barnBuilding)
isBarnBuildingPossible :: Applicative f => f BuildingStatus -> f PlantingStatus -> f Bool
isBarnBuildingPossible building planting = (&&) <$> ((== IsNotBuilding) <$> building) <*> ((== IsNotPlanting) <$> planting)

drawBuildingSpace :: PlayerWidget t m x => m (Dynamic t (Maybe WorkerId))
drawBuildingSpace = divAttributeLike buildingSpaceClass $ do
  rec
    (clickedPosition, clickedOccupant, hoveredPosition) <- drawBuildings $ DynamicInfo hoveredPosition selectedOccupantDyn plantingStatusDyn buildingStatusDyn barnBuildingStatusDyn
    selectedOccupantDyn <- createSelectedOccupant clickedOccupant
    occupantChangeActions <- createOccupantChangeActions selectedOccupantDyn clickedPosition buildingStatusDyn plantingStatusDyn
    tellPlayerAction occupantChangeActions
    (plantingStatusDyn, buildingStatusDyn, barnBuildingStatusDyn) <- divAttributeLike buildingOptionsClass $ do
      rec
        let canBuildDyn = isBuildingPossible plantingStatusDynInner barnBuildingStatusDynInner
            canPlantDyn = isPlantingPossible buildingStatusDynInner barnBuildingStatusDynInner
            canBuildBarnDyn = isBarnBuildingPossible buildingStatusDynInner plantingStatusDynInner
        buildingStatusDynInner <- drawBuildingSelection canBuildDyn
        plantingStatusDynInner <- drawPlanting clickedPosition canPlantDyn
        barnBuildingStatusDynInner <- drawBarnBuilding canBuildBarnDyn
        plantActions <- drawPlantingConfirmation plantingStatusDyn
        tellPlayerAction plantActions
      return (plantingStatusDynInner, buildingStatusDynInner, barnBuildingStatusDynInner)
    tellPlayerAction $ createBuildActions buildingStatusDyn clickedPosition
    tellPlayerAction $ createBuildBarnAction barnBuildingStatusDyn clickedPosition
    let selectedWorkerDyn = (workerFromOccupant =<<) <$> selectedOccupantDyn
  return $ selectedWorkerDyn

createBuildBarnAction :: Reflex t => Dynamic t BarnBuildingStatus -> Event t Position -> Event t PlayerAction
createBuildBarnAction statusDyn posEvent =
  let act IsBuildingBarn pos = Just $ \playerId universe -> buildBarn playerId pos universe
      act _ _ = Nothing
  in attachWithMaybe act (current statusDyn) posEvent

drawOccupantErrors :: (MonadWidget t m) => [String] -> m ()
drawOccupantErrors errors =
  forM_ errors $ \error ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (text $ T.pack $ error)

drawRotationButton :: PlayerWidget t m x => m (Dynamic t Direction)
drawRotationButton = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  currentBuildingDyn <- holdUniqDyn $ currentlyBuiltBuildings <$> universeDyn <*> pure playerId
  let getClass [] = hiddenButtonClass
      getClass _ = rotateButtonClass
  (rotateElement, _) <- divAttributeLikeDyn' (getClass <$> currentBuildingDyn) $ return ()
  let rotateClicks = domEvent Click rotateElement
  foldDyn (const nextDirection) DirectionDown rotateClicks

drawSelectionForPossibilities :: MonadWidget t m => [BuildingDescription] -> m (Dynamic t (Maybe BuildingDescription))
drawSelectionForPossibilities [] = return $ pure Nothing
drawSelectionForPossibilities possibilities = do
  let possibilitiesCount = length possibilities
      drawTile buildingDescription = do
        divAttributeLikeDyn (buildingSelectionCss . getTileBuildingType DirectionRight ExactPosition <$> buildingDescription) $ return ()
        divAttributeLikeDyn (buildingSelectionCss . getTileBuildingType DirectionRight NextPosition <$> buildingDescription) $ return ()
  rec
    (leftEl, _) <- divAttributeLike' switchBuildingLeftClass $ return ()
    drawTile currentBuilding
    (rightEl, _) <- divAttributeLike' switchBuildingRightClass $ return ()
    let switchEvent = leftmost [const (-1 :: Int) <$> domEvent Click leftEl, const (1 :: Int) <$> domEvent Click rightEl]
    currentIndex <- foldDyn (\x y -> mod (x + y) possibilitiesCount) (0 :: Int) switchEvent
    let currentBuilding = (possibilities !!) <$> currentIndex
  return $ Just <$> currentBuilding

createSelectedOccupant :: PlayerWidget t m x => Event t BuildingOccupant -> m (Dynamic t (Maybe BuildingOccupant))
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
  holdUniqDyn selectedOccupants

isOccupantValid :: BuildingOccupant -> Universe -> Bool
isOccupantValid (WorkerOccupant workerId) universe = isNothing (getWorkerWorkplace universe workerId)
isOccupantValid _ _ = True

drawWorkplaceOccupant :: PlayerWidget t m x => Dynamic t (Maybe BuildingOccupant) -> BuildingOccupant -> m (Event t BuildingOccupant)
drawWorkplaceOccupant selectedOccupant (WorkerOccupant workerId) = do
  let selectedWorker = (workerFromOccupant =<<) <$> selectedOccupant
  workerEvent <- drawWorker selectedWorker workerId
  return $ WorkerOccupant <$> workerEvent
drawWorkplaceOccupant selectedOccupant occ@(AnimalOccupant (Animal animalType _)) = do
  let cls selOcc = if selOcc == Just occ then highlightedAnimalClass <> animalClass else animalClass
  (divEl, _) <- divAttributeLikeDyn' (cls <$> selectedOccupant) $ text (T.pack $ show animalType)
  return $ const occ <$> domEvent Click divEl

findOccupantChanges :: Reflex t => Dynamic t (Maybe BuildingOccupant) -> Event t Position -> Event t (BuildingOccupants -> BuildingOccupants)
findOccupantChanges selectedOccupant clickedPosition =
  let removeOccupant occupant = M.map (Prelude.filter (/= occupant))
      addOccupant occupant = M.alter (pure . (occupant:) . fromMaybe [])
      modifyMap (Just occ) pos = addOccupant occ pos . removeOccupant occ
      modifyMap _ _ = id
  in attachWith modifyMap (current selectedOccupant) clickedPosition

createOccupantChangeActions :: PlayerWidget t m x => Dynamic t (Maybe BuildingOccupant) -> Event t Position -> Dynamic t BuildingStatus -> Dynamic t PlantingStatus -> m (Event t PlayerAction)
createOccupantChangeActions selectedOccupant clickedPositionEvent buildingStatusDyn plantingStatusDyn = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let occupantChanges = findOccupantChanges selectedOccupant clickedPositionEvent
      updatedOccupantsEvent = attachWith (&) (current $ getBuildingOccupants <$> universeDyn <*> pure playerId) occupantChanges
      filterBuilding IsNotBuilding ev = Just ev
      filterBuilding _ _ = Nothing
      filterPlanting IsNotPlanting ev = Just ev
      filterPlanting _ _ = Nothing
      createAction occ = \plId -> alterOccupants plId occ
  return $
    attachWithMaybe filterPlanting (current plantingStatusDyn) $
    attachWithMaybe filterBuilding (current buildingStatusDyn) $
    createAction <$> updatedOccupantsEvent

workerFromOccupant :: BuildingOccupant -> Maybe WorkerId
workerFromOccupant (WorkerOccupant workerId) = Just workerId
workerFromOccupant _ = Nothing

nextDirection :: Direction -> Direction
nextDirection DirectionUp = DirectionRight
nextDirection DirectionRight = DirectionDown
nextDirection DirectionDown = DirectionLeft
nextDirection DirectionLeft = DirectionUp
