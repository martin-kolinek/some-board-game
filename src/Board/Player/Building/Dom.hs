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
import Data.Map as M
import Data.Maybe
import Control.Arrow ((***))
import Data.Tuple
import Data.Monoid
import Data.Either
import Data.AdditiveGroup
import Control.Monad.IO.Class
import Prelude hiding (error)
import qualified Data.List as L

drawBuildingSpace :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (PlayerExports t)
drawBuildingSpace playerId = divAttributeLike buildingSpaceClass $ do
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
drawBuildings buildings =
  forM_ buildings $ \building ->
    divAttributeLike (buildingCss building) $ return ()

drawBuildingOccupants :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => PlayerId -> m (Dynamic t (Maybe WorkerId), Event t (BuildingOccupants -> BuildingOccupants))
drawBuildingOccupants playerId = do
  universeDyn <- askUniverse
  occupantsDyn <- mapDyn (`getBuildingOccupants` playerId) universeDyn
  let positionErrorsFunc position errors = Prelude.filter ((== position) . snd) errors
  occupantErrors <- mapDyn (`getOccupantErrors` playerId) universeDyn
  rec
    clicks <- forM availableBuildingPositions $ \position -> do
      liftIO (putStrLn ("Pos " ++ (show position)))
      positionOccupants <- mapDyn (findWithDefault [] position) occupantsDyn
      let occupantsFilter occupants universe = [occupant | occupant <- occupants, isOccupantValid occupant universe]
      filteredPositionOccupants <- combineDyn occupantsFilter positionOccupants universeDyn
      positionErrors <- mapDyn (positionErrorsFunc position) occupantErrors
      (positionDiv, insideClicks) <- divAttributeLike' (placeholderTileCss position, placeholderTileClass) $ do
        performEvent_ $ (\e -> liftIO (putStrLn ("Errors "++ (show e)))) <$> (updated positionErrors)
        mapDynExtract drawOccupantErrors positionErrors
        divAttributeLike occupantContainerClass $ do
          let combineOccupantClicks workers = leftmost $ elems workers
          occupantClicks <- animatedList (fromRational 1) filteredPositionOccupants (drawWorkplaceOccupant selectedOccupant)
          combinedClicks <- combineOccupantClicks `mapDyn` occupantClicks
          return $ switch (current combinedClicks)
      return (insideClicks, const position <$> domEvent Click positionDiv)
    let (lastClickedOccupant, lastClickedPosition) = (leftmost *** leftmost) $ unzip clicks
    selectedOccupant <- deselectInvalidOccupants lastClickedOccupant
    selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
    let occupantChanges = findOccupantChanges selectedOccupant lastClickedPosition
  return (selectedWorker, occupantChanges)

drawOccupantErrors :: (MonadWidget t m) => [OccupantError] -> m ()
drawOccupantErrors errors =
  forM_ errors $ \(error, _) ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (text error)

drawPositionSelection :: (UniverseReader t m x, MonadWidget t m) => PlayerStatus -> m (Event t (Position, Direction), Event t ())
drawPositionSelection CuttingForest = drawActivePositionSelection
drawPositionSelection DiggingCave = drawActivePositionSelection
drawPositionSelection DiggingPassage = drawActivePositionSelection
drawPositionSelection _ = return (never, never)

drawActivePositionSelection :: (UniverseReader t m x, MonadWidget t m) => m (Event t (Position, Direction), Event t ())
drawActivePositionSelection = do
  universeDyn <- askUniverse
  (cancelElement, _) <- divAttributeLike' cancelButtonWrapperClass $ divAttributeLike' cancelButtonClass $ return ()
  let cancelClicks = domEvent Click cancelElement
  rec
    (rotateElement, _) <- divAttributeLike' rotateButtonWrapperClass $ divAttributeLike' rotateButtonClass $ return ()
    let rotateClicks = domEvent Click rotateElement
    direction <- foldDyn (const nextDirection) DirectionDown rotateClicks
    combined <- combineDyn3 (,,) universeDyn hoveredPositions direction
    mapDynExtract drawPotentialBuildings combined
    positionData <- forM availableBuildingPositions $ \position -> do
      (element, _) <- divAttributeLike' (placeholderTileCss position) $ return ()
      let positionClicks = const position <$> domEvent Click element
      let positionEnters = const (First (Just position)) <$> domEvent Mouseenter element
      let positionLeaves = const (First Nothing) <$> domEvent Mouseleave element
      hoveredPosition <- holdDyn (First Nothing) (positionEnters <> positionLeaves)
      return (swap <$> attach (current direction) positionClicks, hoveredPosition)
    hoveredPositions <- mapDyn getFirst =<< mconcatDyn (snd <$> positionData)
  return (leftmost (fst <$> positionData), cancelClicks)

drawPotentialBuildings :: MonadWidget t m => (Universe, Maybe Position, Direction) -> m ()
drawPotentialBuildings (universe, (Just position), direction) = case selectPosition position direction universe of
  Left _ -> do
    divAttributeLike (highlightedPlaceholderTileCss position) $ return ()
    divAttributeLike (highlightedPlaceholderTileCss (position ^+^ directionAddition direction)) $ return ()
  Right newUniverse -> do
    let buildings = fromMaybe [] $ do
          plId <- getCurrentPlayer universe
          let newBuildings = getBuildingSpace newUniverse plId
              oldBuildings = getBuildingSpace universe plId
          return $ newBuildings L.\\ oldBuildings
    drawBuildings buildings
    divAttributeLike (highlightedValidPlaceholderTileCss position) $ return ()
    divAttributeLike (highlightedValidPlaceholderTileCss (position ^+^ directionAddition direction)) $ return ()
drawPotentialBuildings _ = return ()

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

isOccupantValid :: BuildingOccupant -> Universe -> Bool
isOccupantValid (WorkerOccupant workerId) universe = isNothing (getWorkerWorkplace universe workerId)

drawWorkplaceOccupant :: (MonadWidget t m, UniverseReader t m x, PlayerSettingsReader t m x) =>
  Dynamic t (Maybe BuildingOccupant) -> BuildingOccupant -> Dynamic t AnimationState -> m (Event t BuildingOccupant)
drawWorkplaceOccupant selectedOccupant (WorkerOccupant workerId) animationState = do
  selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
  workerEvent <- drawWorker selectedWorker workerId animationState
  return $ WorkerOccupant <$> workerEvent

workerFromOccupant :: BuildingOccupant -> Maybe WorkerId
workerFromOccupant (WorkerOccupant workerId) = Just workerId

nextDirection :: Direction -> Direction
nextDirection DirectionUp = DirectionRight
nextDirection DirectionRight = DirectionDown
nextDirection DirectionDown = DirectionLeft
nextDirection DirectionLeft = DirectionUp
