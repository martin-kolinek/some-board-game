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

drawBuildingSpace :: PlayerWidget t m => m (PlayerExports t)
drawBuildingSpace = divAttributeLike buildingSpaceClass $ do
  universe <- askUniverseDyn
  playerId <- askPlayerId
  buildings <- mapDyn (`getBuildingSpace` playerId) universe
  mapDynExtract drawBuildings buildings
  (selectedWorker, _) <- drawBuildingOccupants
  -- currentBuildingOccupants <- mapDyn (`getBuildingOccupants` playerId) universe
  -- let wholeOccupantChanges = attachWith (\a b -> (playerId, b a)) (current currentBuildingOccupants) occupantChanges
  -- possibleBuildings <- mapDyn (`currentlyBuiltBuildings` playerId) universe
  -- (positionSelections, cancels) <- mapDynExtract drawPositionSelection possibleBuildings
  return $ PlayerExports selectedWorker never

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings =
  forM_ buildings $ \building ->
    divAttributeLike (buildingCss building) $ return ()

drawBuildingOccupants :: PlayerWidget t m => m (Dynamic t (Maybe WorkerId), Event t (BuildingOccupants -> BuildingOccupants))
drawBuildingOccupants = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
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

drawPositionSelection :: PlayerWidget t m => [[BuildingType]] -> m (Event t (Position, Direction, [BuildingType]), Event t ())
drawPositionSelection [] = return (never, never)
drawPositionSelection possibleBuildings = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  (cancelElement, _) <- divAttributeLike' cancelButtonWrapperClass $ divAttributeLike' cancelButtonClass $ return ()
  let cancelClicks = domEvent Click cancelElement
  rec
    (rotateElement, _) <- divAttributeLike' rotateButtonWrapperClass $ divAttributeLike' rotateButtonClass $ return ()
    let rotateClicks = domEvent Click rotateElement
    direction <- foldDyn (const nextDirection) DirectionDown rotateClicks
    combined <- combineDyn3 (,,) universeDyn hoveredPositions direction
    mapDynExtract (drawPotentialBuildings playerId $ head possibleBuildings) combined
    positionData <- forM availableBuildingPositions $ \position -> do
      (element, _) <- divAttributeLike' (placeholderTileCss position) $ return ()
      let positionClicks = const position <$> domEvent Click element
      let positionEnters = const (First (Just position)) <$> domEvent Mouseenter element
      let positionLeaves = const (First Nothing) <$> domEvent Mouseleave element
      hoveredPosition <- holdDyn (First Nothing) (positionEnters <> positionLeaves)
      return ((\(a, b) -> (b, a, head possibleBuildings)) <$> attach (current direction) positionClicks, hoveredPosition)
    hoveredPositions <- mapDyn getFirst =<< mconcatDyn (snd <$> positionData)
  return (leftmost (fst <$> positionData), cancelClicks)

drawPotentialBuildings :: MonadWidget t m => PlayerId -> [BuildingType] -> (Universe, Maybe Position, Direction) -> m ()
drawPotentialBuildings playerId building (universe, (Just position), direction) = case buildBuildings playerId position direction building universe of
  Left _ -> do
    divAttributeLike (highlightedPlaceholderTileCss position) $ return ()
    if length building > 1
      then divAttributeLike (highlightedPlaceholderTileCss (position ^+^ directionAddition direction)) $ return ()
      else return ()
  Right newUniverse -> do
    let buildings = fromMaybe [] $ do
          plId <- getCurrentPlayer universe
          let newBuildings = getBuildingSpace newUniverse plId
              oldBuildings = getBuildingSpace universe plId
          return $ newBuildings L.\\ oldBuildings
    drawBuildings buildings
    divAttributeLike (highlightedValidPlaceholderTileCss position) $ return ()
    if length building > 1
      then divAttributeLike (highlightedValidPlaceholderTileCss (position ^+^ directionAddition direction)) $ return ()
      else return ()
drawPotentialBuildings _ _ _ = return ()

findOccupantChanges :: Reflex t => Dynamic t (Maybe BuildingOccupant) -> Event t Position -> Event t (BuildingOccupants -> BuildingOccupants)
findOccupantChanges selectedOccupant clickedPosition =
  let removeOccupant occupant = M.map (Prelude.filter (/= occupant))
      addOccupant occupant = alter (pure . (occupant:) . fromMaybe [])
      modifyMap (Just occ) pos = addOccupant occ pos . removeOccupant occ
      modifyMap _ _ = id
  in attachWith modifyMap (current selectedOccupant) clickedPosition

deselectInvalidOccupants :: PlayerWidget t m => Event t BuildingOccupant -> m (Dynamic t (Maybe BuildingOccupant))
deselectInvalidOccupants occupants = do
  let removeInvalidOccupants maybeOccupant universe = do
          occupant <- maybeOccupant
          guard $ isOccupantValid occupant universe
          return occupant
      justOccupants = Just <$> occupants
  heldOccupants <- hold Nothing justOccupants
  universeChangeEvents <- updated <$> askUniverseDyn
  let filteredSelection = attachWith removeInvalidOccupants heldOccupants universeChangeEvents
  selectedOccupants <- holdDyn Nothing $ leftmost [filteredSelection, justOccupants]
  return (nubDyn selectedOccupants)

isOccupantValid :: BuildingOccupant -> Universe -> Bool
isOccupantValid (WorkerOccupant workerId) universe = isNothing (getWorkerWorkplace universe workerId)
isOccupantValid _ _ = True

drawWorkplaceOccupant :: PlayerWidget t m =>
  Dynamic t (Maybe BuildingOccupant) -> BuildingOccupant -> Dynamic t AnimationState -> m (Event t BuildingOccupant)
drawWorkplaceOccupant selectedOccupant (WorkerOccupant workerId) animationState = do
  selectedWorker <- mapDyn (workerFromOccupant =<<) selectedOccupant
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
