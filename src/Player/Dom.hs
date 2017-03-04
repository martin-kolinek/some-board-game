{-# LANGUAGE RecursiveDo, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TupleSections, TypeFamilies, OverloadedStrings #-}

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
import qualified Data.Text as T
import Data.Monoid

drawPlayersNew :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> m (Event t UniverseAction)
drawPlayersNew universeDyn settingsDyn = do
  let playersDyn = getPlayers <$> universeDyn
  selectedPlayerDyn <- drawPlayerSelection settingsDyn universeDyn
  forDynExtract playersDyn $ \players -> fmap leftmost $ forM players $ \playerId -> do
    let singlePlayerSettingsDyn = (flip singlePlayerSettings playerId) <$> settingsDyn
    playerActionEvent <- flip runReaderT (PlayerWidgetData universeDyn playerId singlePlayerSettingsDyn) $ do
      let cls selPlId = if selPlId == playerId then playerDataContainerClass else hiddenPlayerData
          clsDyn = cls <$> selectedPlayerDyn
      divAttributeLikeDyn clsDyn drawPlayerNew
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
      let players = getPlayers <$> universeDyn
      listOfEvents <- simpleList players $ mapDynExtract (drawPlayer universeDyn settingsDyn selectedPlayer)
      let playerClicks = leftmost <$> listOfEvents
      selectedPlayer <- findSelectedPlayer universeDyn playerClicks
    return selectedPlayer

findSelectedPlayer :: MonadWidget t m => Dynamic t Universe -> Dynamic t (Event t PlayerId) -> m (Dynamic t PlayerId)
findSelectedPlayer universeDyn playerClicks = do
  let currentPlayerDyn = getCurrentPlayer <$> universeDyn
      playersDyn = getPlayers <$> universeDyn
      filterRealPlayerChanges (displayedPlayer, maybeLastPlayer) maybeNextPlayer = do
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
    let defaultPlayer = head <$> playersDyn
        result = fromMaybe <$> defaultPlayer <*> maybePlayerId
  return $ uniqDyn result

drawPlayer :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayer universeDyn settingsDyn selectedPlayerId playerId  = do
  let currentPlayerDyn = getCurrentPlayer <$> universeDyn
      selectedClass isSel = if isSel then selectedPlayerClass else mempty
      drawCurrentPlayerIcon isCur = when isCur $ divAttributeLike currentPlayerIconClass (return ())
      isSelected = (== playerId) <$> selectedPlayerId
      selectedClassDyn = selectedClass <$> isSelected
      isCurrent = (== Just playerId) <$> currentPlayerDyn
      classDyn = mconcat [constDyn playerClass, selectedClassDyn]
  (elem, _) <- divAttributeLikeDyn' classDyn $ do
    let displayName = (playerName . (flip singlePlayerSettings playerId)) <$> settingsDyn
    dynText displayName
    mapDynExtract drawCurrentPlayerIcon isCurrent
  let event = domEvent Click elem
  return $ const playerId <$> event

drawPlayerResources :: PlayerWidget t m => PlayerId -> m ()
drawPlayerResources player = do
  _ <- divAttributeLike' resourcesClass $ do
    resources <- askResources player
    forM_ resourceTypes $ \(resourceFunc, resourceLabel) -> do
      divAttributeLike' () $ do
        let resourceText = (((resourceLabel <> ": ") <>) . T.pack . show . resourceFunc) <$> resources
        dynText resourceText
  return ()

askResources :: PlayerWidget t m => PlayerId -> m (Dynamic t Resources)
askResources player = do
  u <- askUniverseDyn
  return $ getPlayerResources <$> u <*> constDyn player

resourceTypes :: [(Resources -> Int, T.Text)]
resourceTypes = [(getWoodAmount, "Wood"),
                 (getStoneAmount, "Stone"),
                 (getGoldAmount, "Gold"),
                 (getIronAmount, "Iron"),
                 (getWheatAmount, "Wheat"),
                 (getPotatoAmount, "Potato"),
                 (getMoneyAmount, "Money"),
                 (getFoodAmount, "Food")]

