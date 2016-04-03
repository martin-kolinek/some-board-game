{-# LANGUAGE RecursiveDo, FlexibleContexts #-}

module Board.Player.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Worker.Dom
import Board.Player.Style
import Board.Player.Types

import Reflex.Dom
import Data.Maybe
import Data.Map.Strict
import Control.Monad

drawPlayers :: (UniverseReader t m, MonadWidget t m) => m (PlayerExports t)
drawPlayers = do
  selectedPlayer <- drawPlayerSelection
  resourcesDrawn <- mapDyn drawPlayerResources selectedPlayer
  dyn resourcesDrawn
  freeWorkersDrawn <- mapDyn drawFreeWorkers selectedPlayer
  nestedSelectedPlayer <- holdDyn (constDyn Nothing) =<< dyn freeWorkersDrawn
  return $ PlayerExports (joinDyn nestedSelectedPlayer)

drawPlayerSelection :: (UniverseReader t m, MonadWidget t m) => m (Dynamic t PlayerId)
drawPlayerSelection = do
  currentPlayerDyn <- askCurrentPlayer
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      drawCurrentPlayerIcon isCurrent = when isCurrent $ void (divCssClass currentPlayerClass (return ()))
      drawPlayer selectedPlayerId playerId  = do
        playerString <- mapDyn show playerId
        isSelected <- combineDyn (==) playerId selectedPlayerId
        selectedClassDyn <- mapDyn selectedClass isSelected
        isCurrent <- combineDyn ((==) . Just) playerId currentPlayerDyn
        classDyn <- mconcatDyn [constDyn playerClass, selectedClassDyn]
        (el, _) <- divCssClassDyn classDyn $ do
          dynText playerString
          currentIconDrawn <- mapDyn drawCurrentPlayerIcon isCurrent
          dyn currentIconDrawn
        let event = domEvent Click el
        return $ tag (current playerId) event
  rec
    (_, selectedPlayer) <- divCssClass playerContainerClass $ do
      players <- askPlayers
      events <- simpleList players (drawPlayer selectedPlayer)
      combined <- mapDyn leftmost events
      let userSelections = switch (current combined)
          currentPlayerChangeSelections = fmapMaybe id $ updated currentPlayerDyn
      let selections = leftmost [userSelections, currentPlayerChangeSelections]
      maybePlayerId <- holdDyn Nothing (Just <$> selections)
      defaultPlayer <- mapDyn head players
      combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn selectedPlayer

askCurrentPlayer :: (UniverseReader t m, MonadWidget t m) => m (Dynamic t (Maybe PlayerId))
askCurrentPlayer =  join $ mapDyn getCurrentPlayer <$> askUniverse

askPlayers :: (UniverseReader t m, MonadWidget t m) => m (Dynamic t [PlayerId])
askPlayers = join $ mapDyn getPlayers <$> askUniverse

drawPlayerResources :: (UniverseReader t m, MonadWidget t m) => PlayerId -> m ()
drawPlayerResources player = do
  divCssClass resourcesClass $ do
    score <- askScore player
    scoreText <- mapDyn show score
    text "Score: "
    dynText scoreText
  return ()

askScore player = join $ combineDyn getScore <$> askUniverse <*> pure (constDyn player)

freeWorkers :: (UniverseReader t m, MonadWidget t m) => PlayerId -> m (Dynamic t [WorkerId])
freeWorkers player = do
  universe <- askUniverse
  let getFreeWorkers universe player = [w | w <- getWorkers universe player, isNothing $ getWorkerWorkplace universe w]
  mapDyn (flip getFreeWorkers player) universe

drawFreeWorkers :: (UniverseReader t m, MonadWidget t m) => PlayerId -> m (Dynamic t (Maybe WorkerId))
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

deselectActiveWorkers :: (UniverseReader t m, MonadWidget t m) => Event t WorkerId -> m (Dynamic t (Maybe WorkerId))
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
