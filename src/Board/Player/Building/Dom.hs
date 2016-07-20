{-# LANGUAGE RecursiveDo, TupleSections, OverloadedStrings #-}

module Board.Player.Building.Dom where

import Rules
import Types
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
import Data.Tuple
import Data.Monoid
import Data.AdditiveGroup
import Data.Either

drawBuildingSpace :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (PlayerExports t)
drawBuildingSpace playerId = do
  universe <- askUniverse
  buildings <- mapDyn (`getBuildingSpace` playerId) universe
  mapDynExtract drawBuildings buildings
  (selectedWorker, occupantChanges) <- drawBuildingOccupants playerId
  currentBuildingOccupants <- mapDyn (`getBuildingOccupants` playerId) universe
  let wholeOccupantChanges = attachWith (\a b -> (playerId, b a)) (current currentBuildingOccupants) occupantChanges
  playerStatus <- mapDyn (`getPlayerStatus` playerId) universe
  (positionSelections, cancels) <- mapDynExtract drawPositionSelection playerStatus
  return $ PlayerExports selectedWorker wholeOccupantChanges positionSelections cancels

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings = divAttributeLike buildingSpaceClass $
  forM_ buildings $ \building -> do
    let style = styleStringFromCss $ buildingCss building
    elAttr "div" ("style" =: style) $ return ()

drawBuildingOccupants :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (Dynamic t (Maybe WorkerId), Event t (BuildingOccupants -> BuildingOccupants))
drawBuildingOccupants playerId = do
  universe <- askUniverse
  occupants <- mapDyn (`getBuildingOccupants` playerId) universe
  let positionErrorsFunc errors position = Prelude.filter ((== position) . snd) errors
  occupantErrorsByPosition <- mapDyn (positionErrorsFunc . (`getOccupantErrors` playerId)) universe
  rec
    (_, (lastClickedOccupant, lastClickedPosition)) <- divAttributeLike' buildingSpaceClass $ do
      clicks <- forM availableBuildingPositions $ \position -> do
        positionOccupants <- mapDyn (findWithDefault [] position) occupants
        let occupantsFilter occupants universe = [occupant | occupant <- occupants, isOccupantValid occupant universe]
        filteredPositionOccupants <- combineDyn occupantsFilter positionOccupants universe
        positionErrors <- combineDyn id occupantErrorsByPosition (constDyn position)
        (positionDiv, insideClicks) <- elAttr' "div" ("style" =: styleStringFromCss (placeholderTileCss position)) $ do
          mapDynExtract drawOccupantErrors positionErrors
          divAttributeLike occupantContainerClass $ do
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

drawOccupantErrors :: (MonadWidget t m) => [OccupantError] -> m ()
drawOccupantErrors errors =
  forM_ errors $ \(error, position) ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (text error)

drawPositionSelection :: (UniverseReader t m x, MonadWidget t m) => PlayerStatus -> m (Event t (Position, Direction), Event t ())
drawPositionSelection CuttingForest = do
  universeDyn <- askUniverse
  divAttributeLike buildingSpaceClass $ do
    (cancelElement, _) <- divAttributeLike' cancelButtonWrapperClass $ divAttributeLike' cancelButtonClass $ return ()
    let cancelClicks = domEvent Click cancelElement
    rec
      (rotateElement, _) <- divAttributeLike' rotateButtonWrapperClass $ divAttributeLike' rotateButtonClass $ return ()
      let rotateClicks = domEvent Click rotateElement
      direction <- foldDyn (const nextDirection) DirectionDown rotateClicks
      positionData <- forM availableBuildingPositions $ \position -> do
        let isPositionHighlighted hoveredPosition rotation currentPosition =
                hoveredPosition == Just currentPosition ||
                  Just currentPosition == ((^+^ directionAddition rotation) <$> hoveredPosition)
            isPositionAllowed (Just position) rotation universe = isRight $ selectPosition position rotation universe
            isPositionAllowed _ _ _ = False
            styleFunc pos rot un
              | isPositionHighlighted pos rot position && isPositionAllowed pos rot un = "style" =: styleStringFromCss (highlightedValidPlaceholderTileCss position)
              | isPositionHighlighted pos rot position = "style" =: styleStringFromCss (highlightedPlaceholderTileCss position)
              | otherwise = "style" =: styleStringFromCss (placeholderTileCss position)
        styleDyn <- combineDyn3 styleFunc hoveredPositions direction universeDyn
        (element, _) <- elDynAttr' "div" styleDyn $ return ()
        let positionClicks = const position <$> domEvent Click element
        let positionEnters = const (First (Just position)) <$> domEvent Mouseenter element
        let positionLeaves = const (First Nothing) <$> domEvent Mouseleave element
        hoveredPosition <- holdDyn (First Nothing) (positionEnters <> positionLeaves)
        return (swap <$> attach (current direction) positionClicks, hoveredPosition)
      hoveredPositions <- mapDyn getFirst =<< mconcatDyn (snd <$> positionData)
    return (leftmost (fst <$> positionData), cancelClicks)
drawPositionSelection _ = return (never, never)

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

nextDirection DirectionUp = DirectionRight
nextDirection DirectionRight = DirectionDown
nextDirection DirectionDown = DirectionLeft
nextDirection DirectionLeft = DirectionUp
