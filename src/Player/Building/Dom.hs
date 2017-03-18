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

data PotentialBuilding = ValidBuilding BuildingType | InvalidBuilding BuildingType | NoBuilding

data TileInfo = TileInfo
  {
    tileBuilding :: BuildingType,
    tileOccupants :: [BuildingOccupant],
    tileOccupantErrors :: [String],
    tilePotentialBuildings :: PotentialBuilding
  }

createTiles :: Universe -> PlayerId -> Maybe Position -> Direction -> [BuildingType] -> M.Map Position TileInfo
createTiles universe playerId hoveredPositionMaybe hoveredDirection currentBuildings =
  let buildings = getBuildingSpace universe playerId
      getBuiltBuildingIndex position = if Just position == hoveredPositionMaybe then 0 else if Just (position ^-^ directionAddition hoveredDirection) == hoveredPositionMaybe then 1 else 2
      getPotentialBuildingType position = listToMaybe $ drop (getBuiltBuildingIndex position) currentBuildings
      getPotentialBuilding position = case (getPotentialBuildingType position, hoveredPositionMaybe) of
        (Just bt, Just hoveredPosition) -> if isLeft (buildBuildings playerId hoveredPosition hoveredDirection currentBuildings universe) then InvalidBuilding bt else ValidBuilding bt
        _ -> NoBuilding
      getTileOccupants position = findVisibleOccupants universe playerId position
      getTileOccupantErrors position = fst <$> (filter ((== position) . snd) $ getOccupantErrors universe playerId)
      getBuildingTile (Building buildingType pos) = (pos, TileInfo buildingType (getTileOccupants pos) (getTileOccupantErrors pos) (getPotentialBuilding pos))
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
  let getBuildingToDraw tileInfo = case tilePotentialBuildings tileInfo of
        NoBuilding -> tileBuilding tileInfo
        ValidBuilding b -> b
        InvalidBuilding b -> b
      getOverlayCss tileInfo = case tilePotentialBuildings tileInfo of
        NoBuilding -> hiddenPlaceholderTileCss
        ValidBuilding _ -> highlightedValidPlaceholderTileCss
        InvalidBuilding _ -> highlightedPlaceholderTileCss
  (divEl, inner) <- divAttributeLikeDyn' (flip buildingCss2 position <$> getBuildingToDraw <$> tileInfoDyn) $ do
    divAttributeLikeDyn (getOverlayCss <$> tileInfoDyn) $ do
      divAttributeLike occupantContainerClass $ do
        let combineOccupantClicks workers = M.elems <$> mergeMap workers
        occupantClicks <- animatedList (fromRational 1) (tileOccupants <$> tileInfoDyn) (drawWorkplaceOccupant selectedOccupantDyn)
        let combinedClicks = combineOccupantClicks <$> occupantClicks
        return $ switch (current combinedClicks)
  hoveredPos <- holdDyn [] (leftmost [const [] <$> domEvent Mouseleave divEl, const [position] <$> domEvent Mouseenter divEl])
  return $ TileResults (const [position] <$> domEvent Click divEl) inner hoveredPos

drawBuildingSpaceNew :: PlayerWidget t m => m (PlayerExports t)
drawBuildingSpaceNew = divAttributeLike buildingSpaceClass $ do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  directionDyn <- drawRotationButton
  let buildingDyn = fmap (fromMaybe []) $ fmap listToMaybe $ currentlyBuiltBuildings <$> universeDyn <*> pure playerId
  rec
    let tiles = createTiles <$> universeDyn <*> pure playerId <*> (listToMaybe <$> hoveredPositionDyn) <*> directionDyn <*> buildingDyn
    result <- listWithKey tiles (drawTileInfo selectedOccupant)
    (TileResults clickedPos clickedOcc hoveredPositionDyn) <- extractTileResultsFromDynamic $ fold <$> result
    selectedOccupant <- deselectInvalidOccupants (fmapMaybe listToMaybe clickedOcc)
    let selectedWorker = (workerFromOccupant =<<) <$> selectedOccupant
        clickedPosition = fmapMaybe listToMaybe clickedPos
        occupantChanges = findOccupantChanges selectedOccupant clickedPosition
        wholeOccupantChanges = attachWith (&) (current (getBuildingOccupants <$> universeDyn <*> pure playerId)) occupantChanges
        occupantChangeActions = flip alterOccupants <$> wholeOccupantChanges
        createBuildAction :: [BuildingType] -> Direction -> Position -> PlayerAction
        createBuildAction buildingType direction position plId = buildBuildings plId position direction buildingType
        buildBuildingActions = attachWith ($) (createBuildAction <$> (current buildingDyn) <*> (current directionDyn)) clickedPosition
  return $ PlayerExports selectedWorker (leftmost [buildBuildingActions, occupantChangeActions])

drawOccupantErrors :: (MonadWidget t m) => [OccupantError] -> m ()
drawOccupantErrors errors =
  forM_ errors $ \(error, _) ->
    divAttributeLike occupantErrorClass $ do
      divAttributeLike occupantErrorIconClass (return ())
      divAttributeLike occupantErrorTextClass (text $ T.pack error)

drawRotationButton :: MonadWidget t m => m (Dynamic t Direction)
drawRotationButton = do
  (rotateElement, _) <- divAttributeLike' rotateButtonWrapperClass $ divAttributeLike' rotateButtonClass $ return ()
  let rotateClicks = domEvent Click rotateElement
  foldDyn (const nextDirection) DirectionDown rotateClicks

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
