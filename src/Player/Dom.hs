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
import Data.Map (fromList, foldl')
import Control.Monad
import Control.Monad.Reader
import Prelude hiding (elem)
import qualified Data.Text as T
import Data.Monoid

drawPlayers :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> m (Event t UniverseAction)
drawPlayers universeDyn settingsDyn = do
  let playersDyn = getPlayers <$> universeDyn
  selectedPlayerDyn <- uniqDyn <$> drawPlayerSelection settingsDyn universeDyn
  mapEvent <- listViewWithKey (fromList . (fmap (, Nothing)) <$> playersDyn) $ \playerId _ -> do
    playerActionEvent <- flip runReaderT (PlayerWidgetData universeDyn playerId settingsDyn) $ do
      let cls selPlId = if selPlId == playerId then playerDataContainerClass else hiddenPlayerData
          clsDyn = cls <$> selectedPlayerDyn
      divAttributeLikeDyn clsDyn drawPlayer
    return $ ($ playerId) <$> playerActionEvent
  return $ foldl' (>=>) return <$> mapEvent

drawPlayer :: PlayerWidget t m => m (Event t PlayerAction)
drawPlayer = do
  resourceCollectEvents <- drawPlayerResources
  workplaceClicks <- drawWorkplaces
  PlayerExports selectedWorker action <- drawBuildingSpace
  let startWorkerAction (Just worker) workplace = Just $ \player -> startWorking player worker workplace
      startWorkerAction _ _ = Nothing
      startWorkerActionEvent = attachWithMaybe startWorkerAction (current selectedWorker) workplaceClicks
  return $ leftmost [startWorkerActionEvent, action, resourceCollectEvents]

type SelectedPlayerWithSettingsChanges t = (Dynamic t PlayerId, Event t SinglePlayerSettings)

drawPlayerSelection :: MonadWidget t m => Dynamic t PlayerSettings -> Dynamic t Universe -> m (Dynamic t PlayerId)
drawPlayerSelection settingsDyn universeDyn =
  divAttributeLike playerContainerClass $ do
    rec
      let players = getPlayers <$> universeDyn
      listOfEvents <- simpleListOrd players $ (drawPlayerInSelection universeDyn settingsDyn selectedPlayer)
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

drawPlayerInSelection :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayerInSelection universeDyn settingsDyn selectedPlayerId playerId  = do
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
    dyn $ drawCurrentPlayerIcon <$> isCurrent
  let event = domEvent Click elem
  return $ const playerId <$> event

drawPlayerResources :: PlayerWidget t m => m (Event t PlayerAction)
drawPlayerResources = do
  divAttributeLike resourcesClass $ do
    resources <- askResources
    forM_ resourceTypes $ \(resourceFunc, resourceLabel) -> do
      divAttributeLike' () $ do
        let resourceText = (((resourceLabel <> ": ") <>) . T.pack . show . resourceFunc) <$> resources
        dynText resourceText
    collectResourcesEvent <- drawActionButton canCollectResources "Collect resources"
    finishActionEvent <- drawActionButton canFinishAction "Finish action"
    return $ leftmost [const collectResources <$> collectResourcesEvent, const finishAction <$> finishActionEvent]

drawActionButton :: PlayerWidget t m => (Universe -> PlayerId -> Bool) -> T.Text -> m (Event t ())
drawActionButton condition label = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  let condDyn = uniqDyn $ condition <$> universeDyn <*> pure playerId
      draw cond = if cond then button label else return never
  switchPromptly never =<< (dyn $ draw <$> condDyn)

askResources :: PlayerWidget t m => m (Dynamic t Resources)
askResources = do
  u <- askUniverseDyn
  player <- askPlayerId
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

