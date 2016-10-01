{-# LANGUAGE RecursiveDo, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Board.Player.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Player.Style
import Board.Player.Types
import Board.Player.Building.Dom
import Board.Settings.Types

import Reflex.Dom
import Data.Maybe
import Control.Monad
import Prelude hiding (elem)

drawPlayers :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (PlayerExports t)
drawPlayers = do
  selectedPlayer <- drawPlayerSelection
  _ <- mapDynExtract drawPlayerResources selectedPlayer
  mapDynExtract drawBuildingSpace selectedPlayer

type SelectedPlayerWithSettingsChanges t = (Dynamic t PlayerId, Event t SinglePlayerSettings)

drawPlayerSelection :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (Dynamic t PlayerId)
drawPlayerSelection =
  divAttributeLike playerContainerClass $ do
    rec
      players <- askPlayers
      listOfEvents <- simpleList players $ mapDynExtract (drawPlayer selectedPlayer)
      playerClicks <- mapDyn leftmost listOfEvents
      selectedPlayer <- findSelectedPlayer playerClicks
    return selectedPlayer

findSelectedPlayer :: (MonadWidget t m, UniverseReader t m x) => Dynamic t (Event t PlayerId) -> m (Dynamic t PlayerId)
findSelectedPlayer playerClicks = do
  currentPlayerDyn <- askCurrentPlayer
  players <- askPlayers
  let filterRealPlayerChanges (displayedPlayer, maybeLastPlayer) maybeNextPlayer = do
        nextPlayer <- maybeNextPlayer
        guard $ Just nextPlayer /= maybeLastPlayer
        guard $ Just displayedPlayer == maybeLastPlayer
        return nextPlayer
  rec
    let userSelections = switch (current playerClicks)
        displayedPlayerWithCurrentPlayer = (,) <$> current result <*> current currentPlayerDyn
        currentPlayerChangeSelections = attachWithMaybe filterRealPlayerChanges displayedPlayerWithCurrentPlayer (updated currentPlayerDyn)
    delayedCurrentPlayerChangeSelection <- delay (fromRational 0.5) currentPlayerChangeSelections
    let selections = leftmost [userSelections, delayedCurrentPlayerChangeSelection]
    maybePlayerId <- holdDyn Nothing (Just <$> selections)
    defaultPlayer <- mapDyn head players
    result <- combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn result

drawPlayer :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayer selectedPlayerId playerId  = do
  currentPlayerDyn <- askCurrentPlayer
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      drawCurrentPlayerIcon isCurrent = when isCurrent $ divAttributeLike currentPlayerIconClass (return ())
  isSelected <- mapDyn (== playerId) selectedPlayerId
  selectedClassDyn <- mapDyn selectedClass isSelected
  isCurrent <- mapDyn (== Just playerId) currentPlayerDyn
  classDyn <- mconcatDyn [constDyn playerClass, selectedClassDyn]
  (elem, _) <- divAttributeLikeDyn' classDyn $ do
    displayName <- askPlayerName playerId
    dynText displayName
    mapDynExtract drawCurrentPlayerIcon isCurrent
  let event = domEvent Click elem
  return $ const playerId <$> event

askPlayerName :: (MonadWidget t m, PlayerSettingsReader t m x) => PlayerId -> m (Dynamic t String)
askPlayerName playerId = do
  singlePlayerSettingsDyn <- askSinglePlayerSettings playerId
  mapDyn playerName singlePlayerSettingsDyn

askCurrentPlayer :: (UniverseReader t m x, MonadWidget t m) => m (Dynamic t (Maybe PlayerId))
askCurrentPlayer = join $ mapDyn getCurrentPlayer <$> askUniverse

askPlayers :: (UniverseReader t m x, MonadWidget t m) => m (Dynamic t [PlayerId])
askPlayers = join $ mapDyn getPlayers <$> askUniverse

drawPlayerResources :: (UniverseReader t m x, MonadWidget t m) => PlayerId -> m ()
drawPlayerResources player = do
  _ <- divAttributeLike' resourcesClass $ do
    resources <- askResources player
    forM_ resourceTypes $ \resourceFunc -> do
      divAttributeLike' () $ do
        resourceText <- mapDyn (show . resourceFunc) resources
        dynText resourceText
  return ()

askResources :: (MonadWidget t m, UniverseReader t m x) => PlayerId -> m (Dynamic t Resources)
askResources player = join $ combineDyn getPlayerResources <$> askUniverse <*> pure (constDyn player)

resourceTypes :: [Resources -> Int]
resourceTypes = [getWoodAmount,
                 getStoneAmount,
                 getGoldAmount,
                 getIronAmount,
                 getWheatAmount,
                 getPotatoAmount,
                 getDogAmount,
                 getSheepAmount,
                 getPigAmount,
                 getMoney,
                 getFoodAmount]
