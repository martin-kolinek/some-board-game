{-# LANGUAGE RecursiveDo, FlexibleContexts #-}

module Board.Player.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Worker.Dom
import Board.Player.Style

import Reflex.Dom
import Data.Maybe
import Data.Map.Strict

drawPlayerSelection :: (UniverseReader t m, MonadWidget t m) => m (Dynamic t PlayerId)
drawPlayerSelection = do
  universe <- askUniverse
  currentPlayerDyn <- mapDyn getCurrentPlayer universe
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      currentClass isCurrent = if isCurrent then currentPlayerClass else mempty
      drawPlayer selectedPlayerId playerId  = do
        playerString <- mapDyn show playerId
        isSelected <- combineDyn (==) playerId selectedPlayerId
        selectedClassDyn <- mapDyn selectedClass isSelected
        isCurrent <- combineDyn ((==) . Just) playerId currentPlayerDyn
        currentClassDyn <- mapDyn currentClass isCurrent
        classDyn <- mconcatDyn [constDyn playerClass, currentClassDyn, selectedClassDyn]
        scoreDyn <- combineDyn getScore universe playerId
        scoreTextDyn <- mapDyn show scoreDyn
        (el, _) <- divCssClassDyn classDyn $ do
          dynText playerString
          dynText scoreTextDyn
        let event = domEvent Click el
        return $ tag (current playerId) event
  rec
    (_, selectedPlayer) <- divCssClass playerContainerClass $ do
      players <- mapDyn getPlayers universe
      events <- simpleList players (drawPlayer selectedPlayer)
      combined <- mapDyn leftmost events
      let userSelections = switch (current combined)
          currentPlayerChangeSelections = fmapMaybe id $ updated currentPlayerDyn
      let selections = leftmost [userSelections, currentPlayerChangeSelections]
      maybePlayerId <- holdDyn Nothing (Just <$> selections)
      defaultPlayer <- mapDyn head players
      combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn selectedPlayer

freeWorkers :: (UniverseReader t m, MonadWidget t m) => PlayerId -> m (Dynamic t [WorkerId])
freeWorkers player = do
  universe <- askUniverse
  let getFreeWorkers universe player = [w | w <- getWorkers universe player, isNothing $ getWorkerWorkplace universe w]
  mapDyn (flip getFreeWorkers player) universe

drawFreeWorkers :: (UniverseReader t m, MonadWidget t m) => Dynamic t (Maybe WorkerId) -> PlayerId -> m (Event t WorkerId)
drawFreeWorkers selectedWorker player = do
  (_, ev) <- divCssClass freeWorkersClass $ do
    free <- freeWorkers player
    events <- animatedList (fromRational 1) free (drawWorker selectedWorker)
    let combineWorkerClicks :: Reflex t => Map WorkerId (Event t WorkerId) -> Event t WorkerId
        combineWorkerClicks workers = leftmost $ elems workers
    combinedClicks <- combineWorkerClicks `mapDyn` events
    return $ switch (current combinedClicks)
  return ev
