{-# LANGUAGE RecursiveDo, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Board.Player.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Worker.Dom
import Board.Player.Style
import Board.Player.Types
import Board.Settings.Types

import Reflex.Dom
import Data.Maybe
import Data.Map.Strict
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Control.Arrow

drawPlayers :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (PlayerExports t)
drawPlayers = do
  selectedPlayer <- drawPlayerSelection
  resourcesDrawn <- mapDyn drawPlayerResources selectedPlayer
  dyn resourcesDrawn
  freeWorkersDrawn <- mapDyn drawFreeWorkers selectedPlayer
  nestedSelectedPlayer <- holdDyn (constDyn Nothing) =<< dyn freeWorkersDrawn
  return $ PlayerExports (joinDyn nestedSelectedPlayer)

type SelectedPlayerWithSettingsChanges t = (Dynamic t PlayerId, Event t SinglePlayerSettings)

drawPlayerSelection :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (Dynamic t PlayerId)
drawPlayerSelection = do
  (_, result) <- divCssClass playerContainerClass $ do
    rec
      players <- askPlayers
      listOfEvents <- simpleList players $ mapDynExtract (drawPlayer selectedPlayer)
      playerClicks <- mapDyn leftmost listOfEvents
      selectedPlayer <- findSelectedPlayer playerClicks
    return selectedPlayer
  return result

findSelectedPlayer :: (MonadWidget t m, UniverseReader t m x) => Dynamic t (Event t PlayerId) -> m (Dynamic t PlayerId)
findSelectedPlayer playerClicks = do
  currentPlayerDyn <- askCurrentPlayer
  players <- askPlayers
  let userSelections = switch (current playerClicks)
      currentPlayerChangeSelections = fmapMaybe id $ updated currentPlayerDyn
      selections = leftmost [userSelections, currentPlayerChangeSelections]
  maybePlayerId <- holdDyn Nothing (Just <$> selections)
  defaultPlayer <- mapDyn head players
  result <- combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn result

drawPlayer :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayer selectedPlayerId playerId  = do
  currentPlayerDyn <- askCurrentPlayer
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      drawCurrentPlayerIcon isCurrent = when isCurrent $ void (divCssClass currentPlayerIconClass (return ()))
  isSelected <- mapDyn (== playerId) selectedPlayerId
  selectedClassDyn <- mapDyn selectedClass isSelected
  isCurrent <- mapDyn (== Just playerId) currentPlayerDyn
  classDyn <- mconcatDyn [constDyn playerClass, selectedClassDyn]
  (el, settingsVisible) <- divCssClassDyn classDyn $ do
    displayName <- askPlayerName playerId
    dynText displayName
    mapDynExtract drawCurrentPlayerIcon isCurrent
  let event = domEvent Click el
  return $ const playerId <$> event

askPlayerName playerId = do
  singlePlayerSettingsDyn <- askSinglePlayerSettings playerId
  mapDyn playerName singlePlayerSettingsDyn

askCurrentPlayer :: (UniverseReader t m x, MonadWidget t m) => m (Dynamic t (Maybe PlayerId))
askCurrentPlayer = join $ mapDyn getCurrentPlayer <$> askUniverse

askPlayers :: (UniverseReader t m x, MonadWidget t m) => m (Dynamic t [PlayerId])
askPlayers = join $ mapDyn getPlayers <$> askUniverse

drawPlayerResources :: (UniverseReader t m x, MonadWidget t m) => PlayerId -> m ()
drawPlayerResources player = do
  divCssClass resourcesClass $ do
    score <- askScore player
    scoreText <- mapDyn show score
    text "Score: "
    dynText scoreText
  return ()

askScore player = join $ combineDyn getScore <$> askUniverse <*> pure (constDyn player)

freeWorkers :: (UniverseReader t m x, MonadWidget t m) => PlayerId -> m (Dynamic t [WorkerId])
freeWorkers player = do
  universe <- askUniverse
  let getFreeWorkers universe player = [w | w <- getWorkers universe player, isNothing $ getWorkerWorkplace universe w]
  mapDyn (flip getFreeWorkers player) universe

drawFreeWorkers :: (UniverseReader t m x, MonadWidget t m) => PlayerId -> m (Dynamic t (Maybe WorkerId))
drawFreeWorkers playerId = do
  rec
    (_, ev) <- divCssClass freeWorkersClass $ do
      free <- freeWorkers playerId
      events <- animatedList (fromRational 1) free (drawWorker selectedWorker)
      let combineWorkerClicks :: Reflex t => Map WorkerId (Event t WorkerId) -> Event t WorkerId
          combineWorkerClicks workers = leftmost $ elems workers
      combinedClicks <- combineWorkerClicks `mapDyn` events
      return $  switch (current combinedClicks)
    selectedWorker <- deselectActiveWorkers ev
  return selectedWorker

deselectActiveWorkers :: (UniverseReader t m x, MonadWidget t m) => Event t WorkerId -> m (Dynamic t (Maybe WorkerId))
deselectActiveWorkers workers = do
  let removeNonFreeWorkers maybeWorker universe = do
          worker <- maybeWorker
          guard $ isNothing (getWorkerWorkplace universe worker)
          return worker
      justWorkers = Just <$> workers
  heldWorkers <- hold Nothing justWorkers
  universeChangeEvents <- updated <$> askUniverse
  let filteredSelection = attachWith removeNonFreeWorkers heldWorkers universeChangeEvents
  selectedWorkers <- holdDyn Nothing $ leftmost [filteredSelection, justWorkers]
  return (nubDyn selectedWorkers)
