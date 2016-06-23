{-# LANGUAGE RecursiveDo, TupleSections #-}

module Board.Player.Building.Dom where

import Rules
import Types
import Clay hiding (Position, id)
import Common.DomUtil
import Board.Player.Building.Style
import Board.Player.Types
import Board.Worker.Dom
import Board.Settings.Types

import Reflex.Dom
import Control.Monad
import Data.Text.Lazy
import Data.Map as M
import Data.Maybe
import Control.Arrow ((***))

drawBuildingSpace :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (PlayerExports t)
drawBuildingSpace playerId = do
  universe <- askUniverse
  buildings <- mapDyn (`getBuildingSpace` playerId) universe
  mapDynExtract drawBuildings buildings
  buildingOccupants <- mapDyn (`getBuildingOccupants` playerId) universe
  (selectedWorker, occupantChanges) <- drawBuildingOccupants buildingOccupants
  currentBuildingOccupants <- mapDyn (`getBuildingOccupants` playerId) universe
  let wholeOccupantChanges = attachWith (\a b -> (playerId, b a)) (current currentBuildingOccupants) occupantChanges
  playerStatus <- mapDyn (`getPlayerStatus` playerId) universe
  positionSelections <- mapDynExtract drawPositionSelection playerStatus
  return $ PlayerExports selectedWorker wholeOccupantChanges positionSelections

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings = void $ divCssClass buildingSpaceClass $
  forM_ buildings $ \building -> do
    let style = styleStringFromCss $ buildingCss building
    elAttr "div" ("style" =: style) $ return ()

drawBuildingOccupants :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => Dynamic t BuildingOccupants -> m (Dynamic t (Maybe WorkerId), Event t (BuildingOccupants -> BuildingOccupants))
drawBuildingOccupants occupants = do
  universe <- askUniverse
  rec
    (_, (lastClickedOccupant, lastClickedPosition)) <- divCssClass buildingSpaceClass $ do
      clicks <- forM availableBuildingPositions $ \position -> do
        positionOccupants <- mapDyn (findWithDefault [] position) occupants
        let occupantsFilter occupants universe = [occupant | occupant <- occupants, isOccupantValid occupant universe]
        filteredPositionOccupants <- combineDyn occupantsFilter positionOccupants universe
        (positionDiv, insideClicks) <- elAttr' "div" ("style" =: styleStringFromCss (placeholderTileCss position)) $ do
          let combineOccupantClicks workers = leftmost $ elems workers
          occupantClicks <- animatedList (fromRational 1) filteredPositionOccupants (drawWorkplaceOccupant selectedOccupant)
          combinedClicks <- combineOccupantClicks `mapDyn` occupantClicks
          return $ switch (current combinedClicks)
        return (insideClicks, const position <$> domEvent Click positionDiv)
      return $ (leftmost *** leftmost) $ unzip clicks
    selectedOccupant <- deselectInvalidOccupants lastClickedOccupant
    selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
    let occupantChanges = findOccupantChanges selectedOccupant lastClickedPosition
  return (selectedWorker, occupantChanges)

drawPositionSelection :: (MonadWidget t m) => PlayerStatus -> m (Event t Position)
drawPositionSelection CuttingForest = do
  (_, result) <- divCssClass buildingSpaceClass $ do
    events <- forM availableBuildingPositions $ \position -> do
      (element, _) <- elAttr' "div" ("style" =: styleStringFromCss (placeholderTileCss position)) $ return ()
      return $ const position <$> domEvent Click element
    return $ leftmost events
  return result
drawPositionSelection _ = return never

findOccupantChanges :: Reflex t => Dynamic t (Maybe BuildingOccupant) -> Event t Position -> Event t (BuildingOccupants -> BuildingOccupants)
findOccupantChanges selectedOccupant clickedPosition =
  let removeOccupant occupant = M.map (Prelude.filter (/= occupant))
      addOccupant occupant = alter (pure . (occupant:) . fromMaybe [])
      modifyMap (Just occ) pos = addOccupant occ pos . removeOccupant occ
      modifyMap _ _ = id
  in attachWith modifyMap (current selectedOccupant) clickedPosition

deselectInvalidOccupants :: (UniverseReader t m x, MonadWidget t m) => Event t BuildingOccupant -> m (Dynamic t (Maybe BuildingOccupant))
deselectInvalidOccupants occupants = do
  let removeInvalidOccupants maybeOccupant universe = do
          occupant <- maybeOccupant
          guard $ isOccupantValid occupant universe
          return occupant
      justOccupants = Just <$> occupants
  heldOccupants <- hold Nothing justOccupants
  universeChangeEvents <- updated <$> askUniverse
  let filteredSelection = attachWith removeInvalidOccupants heldOccupants universeChangeEvents
  selectedOccupants <- holdDyn Nothing $ leftmost [filteredSelection, justOccupants]
  return (nubDyn selectedOccupants)

isOccupantValid (WorkerOccupant workerId) universe = isNothing (getWorkerWorkplace universe workerId)

drawWorkplaceOccupant :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Dynamic t (Maybe BuildingOccupant) -> BuildingOccupant -> Dynamic t AnimationState -> m (Event t BuildingOccupant)
drawWorkplaceOccupant selectedOccupant (WorkerOccupant workerId) animationState = do
  selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
  workerEvent <- drawWorker selectedWorker workerId animationState
  return $ WorkerOccupant <$> workerEvent

workerFromOccupant (WorkerOccupant workerId) = Just workerId

buildingCss (Grass position) = backgroundColor lightgreen >> positionCss position >> commonBuildingCss
buildingCss (Forest position) = backgroundColor darkgreen >> positionCss position >> commonBuildingCss
buildingCss (Rock position) = backgroundColor gray >> positionCss position >> commonBuildingCss
buildingCss (InitialRoom position) = backgroundColor red >> positionCss position >> commonBuildingCss

commonBuildingCss = width (em 12) >> height (em 12) >> position absolute >> borderColor black >> borderWidth 1 >> borderStyle solid

positionCss (x, y) = left (em $ fromIntegral x*12) >> top (em $ fromIntegral y*12)

placeholderTileCss position = positionCss position >> commonBuildingCss
