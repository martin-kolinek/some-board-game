{-# LANGUAGE RecursiveDo, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Player.Dom where

import Rules
import Types
import Common.DomUtil
import Player.Style
import Player.Types
import Settings.Types
import Player.Board.Dom
import Player.Building.Dom

import Reflex.Dom
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Prelude hiding (elem)
import qualified Clay as C

drawPlayersNew :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> m (Event t UniverseAction)
drawPlayersNew universeDyn settingsDyn = do
  playersDyn <- mapDyn getPlayers universeDyn
  selectedPlayerDyn <- drawPlayerSelection settingsDyn universeDyn
  forDynExtract playersDyn $ \players -> fmap leftmost $ forM players $ \playerId -> do
    singlePlayerSettingsDyn <- mapDyn (flip singlePlayerSettings playerId) settingsDyn
    playerActionEvent <- flip runReaderT (PlayerWidgetData universeDyn playerId singlePlayerSettingsDyn) $ do
      let style selPlId = if selPlId == playerId then C.display C.none else C.display C.block
      styleDyn <- mapDyn style selectedPlayerDyn
      divAttributeLikeDyn styleDyn drawPlayerNew
    return $ ($ playerId) <$> playerActionEvent

drawPlayerNew :: PlayerWidget t m => m (Event t PlayerAction)
drawPlayerNew = do
  workplaceClicks <- drawWorkplaces
  PlayerExports selectedWorker action <- drawBuildingSpace
  let startWorkerAction (Just worker) workplace = Just $ \player -> startWorking player worker workplace
      startWorkerAction _ _ = Nothing
      startWorkerActionEvent = attachWithMaybe startWorkerAction (current selectedWorker) workplaceClicks
  return $ leftmost [startWorkerActionEvent, action]

type SelectedPlayerWithSettingsChanges t = (Dynamic t PlayerId, Event t SinglePlayerSettings)

drawPlayerSelection :: MonadWidget t m => Dynamic t PlayerSettings -> Dynamic t Universe -> m (Dynamic t PlayerId)
drawPlayerSelection settingsDyn universeDyn =
  divAttributeLike playerContainerClass $ do
    rec
      players <- mapDyn getPlayers universeDyn
      listOfEvents <- simpleList players $ mapDynExtract (drawPlayer universeDyn settingsDyn selectedPlayer)
      playerClicks <- mapDyn leftmost listOfEvents
      selectedPlayer <- findSelectedPlayer universeDyn playerClicks
    return selectedPlayer

findSelectedPlayer :: MonadWidget t m => Dynamic t Universe -> Dynamic t (Event t PlayerId) -> m (Dynamic t PlayerId)
findSelectedPlayer universeDyn playerClicks = do
  currentPlayerDyn <- mapDyn getCurrentPlayer universeDyn
  playersDyn <- mapDyn getPlayers universeDyn
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
    defaultPlayer <- mapDyn head playersDyn
    result <- combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn result

drawPlayer :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayer universeDyn settingsDyn selectedPlayerId playerId  = do
  currentPlayerDyn <- mapDyn getCurrentPlayer universeDyn
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      drawCurrentPlayerIcon isCurrent = when isCurrent $ divAttributeLike currentPlayerIconClass (return ())
  isSelected <- mapDyn (== playerId) selectedPlayerId
  selectedClassDyn <- mapDyn selectedClass isSelected
  isCurrent <- mapDyn (== Just playerId) currentPlayerDyn
  classDyn <- mconcatDyn [constDyn playerClass, selectedClassDyn]
  (elem, _) <- divAttributeLikeDyn' classDyn $ do
    displayName <- mapDyn (playerName . (flip singlePlayerSettings playerId)) settingsDyn
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
    forM_ resourceTypes $ \(resourceFunc, resourceLabel) -> do
      divAttributeLike' () $ do
        resourceText <- mapDyn (((resourceLabel ++ ": ") ++) .show . resourceFunc) resources
        dynText resourceText
  return ()

askResources :: (MonadWidget t m, UniverseReader t m x) => PlayerId -> m (Dynamic t Resources)
askResources player = join $ combineDyn getPlayerResources <$> askUniverse <*> pure (constDyn player)

resourceTypes :: [(Resources -> Int, String)]
resourceTypes = [(getWoodAmount, "Wood"),
                 (getStoneAmount, "Stone"),
                 (getGoldAmount, "Gold"),
                 (getIronAmount, "Iron"),
                 (getWheatAmount, "Wheat"),
                 (getPotatoAmount, "Potato"),
                 (getMoneyAmount, "Money"),
                 (getFoodAmount, "Food")]

