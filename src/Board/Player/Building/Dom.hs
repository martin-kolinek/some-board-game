{-# LANGUAGE RecursiveDo #-}

module Board.Player.Building.Dom where

import Rules
import Types
import Clay
import Common.DomUtil
import Board.Player.Building.Style
import Board.Worker.Dom
import Board.Settings.Types

import Reflex.Dom
import Control.Monad
import Data.Text.Lazy
import Data.Map
import Data.Maybe

drawBuildingSpace :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (Dynamic t (Maybe WorkerId))
drawBuildingSpace playerId = do
  universe <- askUniverse
  buildings <- mapDyn (`getBuildingSpace` playerId) universe
  mapDynExtract drawBuildings buildings
  buildingOccupants <- mapDyn (`getBuildingOccupants` playerId) universe
  drawBuildingOccupants buildingOccupants

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings = void $ divCssClass buildingSpaceClass $
  forM_ buildings $ \building -> do
    let style = styleStringFromCss $ buildingCss building
    elAttr "div" ("style" =: style) $ return ()

drawBuildingOccupants :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => Dynamic t BuildingOccupants -> m (Dynamic t (Maybe WorkerId))
drawBuildingOccupants occupants = do
  rec
    (_, lastClickedOccupant) <- divCssClass buildingSpaceClass $ do
      clicks <- forM availableBuildingPositions $ \position -> do
        positionOccupants <- mapDyn (findWithDefault [] position) occupants
        elAttr "div" ("style" =: styleStringFromCss (placeholderTileCss position)) $ do
          let combineOccupantClicks workers = leftmost $ elems workers
          occupantClicks <- animatedList (fromRational 1) positionOccupants (drawWorkplaceOccupant selectedOccupant)
          combinedClicks <- combineOccupantClicks `mapDyn` occupantClicks
          return $ switch (current combinedClicks)
      return $ leftmost clicks
    selectedOccupant <- deselectInvalidOccupants lastClickedOccupant
    selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
  return selectedWorker

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

buildingCss (Grass position) = backgroundColor green >> positionCss position >> commonBuildingCss
buildingCss (Forest position) = backgroundColor darkgreen >> positionCss position >> commonBuildingCss
buildingCss (Rock position) = backgroundColor gray >> positionCss position >> commonBuildingCss
buildingCss (InitialRoom position) = backgroundColor red >> positionCss position >> commonBuildingCss

commonBuildingCss = width (em 12) >> height (em 12) >> position absolute >> borderColor black >> borderWidth 1 >> borderStyle solid

positionCss (x, y) = left (em $ fromIntegral x*12) >> top (em $ fromIntegral y*12)

placeholderTileCss position = positionCss position >> commonBuildingCss
