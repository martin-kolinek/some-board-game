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
import Control.Arrow ((***))
import Data.Tuple
import Data.Monoid
import Data.Either
import Data.AdditiveGroup
import Prelude hiding (error)
import qualified Data.List as L
import qualified Data.Text as T

data PotentialBuilding = ValidBuilding BuildingType | InvalidBuilding BuildingType | NoBuilding

data TileInfo = TileInfo
  {
    tileBuilding :: BuildingType,
    tileOccupants :: [BuildingOccupant],
    tileOccupantErrors :: [String],
    tilePotentialBuilding :: PotentialBuilding
  }

createTiles :: Universe -> PlayerId -> Position -> Direction -> [BuildingType] -> M.Map Position TileInfo
createTiles universe playerId hoveredPosition hoveredDirection currentBuildings =
  let buildings = getBuildingSpace universe playerId
      getTileOccupants position = M.findWithDefault [] position $ getBuildingOccupants universe playerId
      getBuiltBuildingIndex position = if position == hoveredPosition then 0 else if position == hoveredPosition ^+^ directionAddition hoveredDirection then 1 else 2
      getPotentialBuildingType position = listToMaybe $ drop (getBuiltBuildingIndex position) currentBuildings
      getPotentialBuilding position = case getPotentialBuildingType position of
        Nothing -> NoBuilding
        Just bt -> if isLeft (buildBuildings playerId hoveredPosition hoveredDirection currentBuildings universe) then InvalidBuilding bt else ValidBuilding bt
      getTileOccupantErrors position = fst <$> (filter ((== position) . snd) $ getOccupantErrors universe playerId)
      getBuildingTile (Building buildingType pos) = (pos, TileInfo buildingType (getTileOccupants pos) (getTileOccupantErrors pos) (getPotentialBuilding pos))
  in M.fromList $ getBuildingTile <$> buildings

drawBuildingSpace :: PlayerWidget t m => m (PlayerExports t)
drawBuildingSpace = divAttributeLike buildingSpaceClass $ do
  universe <- askUniverseDyn
  playerId <- askPlayerId
  let buildings = (`getBuildingSpace` playerId) <$> universe
  _ <- dyn $ drawBuildings <$> buildings
  (selectedWorker, occupantChanges) <- drawBuildingOccupants
  let currentBuildingOccupants = (`getBuildingOccupants` playerId) <$> universe
      wholeOccupantChanges = attachWith (&) (current currentBuildingOccupants) occupantChanges
      occupantChangeActions = flip alterOccupants <$> wholeOccupantChanges
      possibleBuildings = (`currentlyBuiltBuildings` playerId) <$> universe
  positionSelections <- switchPromptly never =<< (dyn $ drawPositionSelection <$> possibleBuildings)
  let createBuildAction (pos, dir, selectedBuildings) = \plId -> buildBuildings plId pos dir selectedBuildings
      buildActions = createBuildAction <$> positionSelections
  return $ PlayerExports selectedWorker $ leftmost [occupantChangeActions, buildActions]

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings =
  forM_ buildings $ \building ->
    divAttributeLike (buildingCss building) $ return ()

drawBuildingOccupants :: PlayerWidget t m => m (Dynamic t (Maybe WorkerId), Event t (BuildingOccupants -> BuildingOccupants))
drawBuildingOccupants = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let occupantsDyn = (`getBuildingOccupants` playerId) <$> universeDyn
      positionErrorsFunc position errors = Prelude.filter ((== position) . snd) errors
      occupantErrors = (`getOccupantErrors` playerId) <$> universeDyn
  rec
    clicks <- forM availableBuildingPositions $ \position -> do
      let positionOccupants = (M.findWithDefault [] position) <$> occupantsDyn
          occupantsFilter occupants universe = [occupant | occupant <- occupants, isOccupantValid occupant universe]
          filteredPositionOccupants = occupantsFilter <$> positionOccupants <*> universeDyn
          positionErrors = (positionErrorsFunc position) <$> occupantErrors
      (positionDiv, insideClicks) <- divAttributeLike' (placeholderTileCss position, placeholderTileClass) $ do
        _ <- dyn $ drawOccupantErrors <$> positionErrors
        divAttributeLike occupantContainerClass $ do
          let combineOccupantClicks workers = leftmost $ M.elems workers
          occupantClicks <- animatedList (fromRational 1) filteredPositionOccupants (drawWorkplaceOccupant selectedOccupant)
          let combinedClicks = combineOccupantClicks <$> occupantClicks
          return $ switch (current combinedClicks)
      return (insideClicks, const position <$> domEvent Click positionDiv)
    let (lastClickedOccupant, lastClickedPosition) = (leftmost *** leftmost) $ unzip clicks
    selectedOccupant <- deselectInvalidOccupants lastClickedOccupant
    let selectedWorker = (workerFromOccupant =<<) <$> selectedOccupant
        occupantChanges = findOccupantChanges selectedOccupant lastClickedPosition
  return (selectedWorker, occupantChanges)

drawOccupantErrors :: (MonadWidget t m) => [OccupantError] -> m ()
drawOccupantErrors errors =
  forM_ errors $ \(error, _) ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (text $ T.pack error)

drawPositionSelection :: PlayerWidget t m => [[BuildingType]] -> m (Event t (Position, Direction, [BuildingType]))
drawPositionSelection [] = return never
drawPositionSelection possibleBuildings = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  rec
    (rotateElement, _) <- divAttributeLike' rotateButtonWrapperClass $ divAttributeLike' rotateButtonClass $ return ()
    let rotateClicks = domEvent Click rotateElement
    direction <- foldDyn (const nextDirection) DirectionDown rotateClicks
    let combined = (,,) <$> universeDyn <*> hoveredPositions <*> direction
    _ <- dyn $ (drawPotentialBuildings playerId $ head possibleBuildings) <$> combined
    positionData <- forM availableBuildingPositions $ \position -> do
      (placeholderElem, _) <- divAttributeLike' (placeholderTileCss position) $ return ()
      let positionClicks = const position <$> domEvent Click placeholderElem
      let positionEnters = const (First (Just position)) <$> domEvent Mouseenter placeholderElem
      let positionLeaves = const (First Nothing) <$> domEvent Mouseleave placeholderElem
      hoveredPosition <- holdDyn (First Nothing) (positionEnters <> positionLeaves)
      return ((\(a, b) -> (b, a, head possibleBuildings)) <$> attach (current direction) positionClicks, hoveredPosition)
    let hoveredPositions = getFirst <$> mconcat (snd <$> positionData)
  return $ leftmost (fst <$> positionData)

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
      addOccupant occupant = M.alter (pure . (occupant:) . fromMaybe [])
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
