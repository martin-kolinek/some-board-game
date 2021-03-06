{-# LANGUAGE RecursiveDo, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TupleSections, TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}

module Player.Dom where

import Rules
import Types
import Common.DomUtil
import Player.Style
import Player.Types
import Settings.Types
import Player.Board.Dom
import Player.Building.Dom
import Player.Cheats
import Player.Resources.Dom

import Reflex.Dom
import Data.Maybe
import Data.Map (fromList)
import Control.Monad
import Control.Monad.Reader
import Prelude hiding (elem)
import qualified Data.Text as T
import Text.Read

drawPlayers :: forall t m x. UniverseWidget t m x => m ()
drawPlayers = do
  universeDyn <- askUniverseDyn
  let playersDyn = getPlayers <$> universeDyn
  selectedPlayerDyn <- drawPlayerSelection
  _ <- listWithKey (fromList . (fmap (, ())) <$> playersDyn) $ \playerId _ -> do
    universeContext <- (asks getUniverseContext :: m (UniverseContext t))
    flip runReaderT (PlayerContext universeContext playerId) $ do
      let cls selPlId = if selPlId == playerId then playerDataContainerClass else hiddenPlayerData
          clsDyn = cls <$> selectedPlayerDyn
      divAttributeLikeDyn clsDyn drawPlayer
      whenWidget ((== playerId) <$> selectedPlayerDyn) cheatsHandler
    return ()
  return ()

drawPlayer :: PlayerWidget t m x => m ()
drawPlayer = do
  drawPlayerInfo
  workplaceClicks <- drawWorkplaces
  selectedWorker <- drawBuildingSpace
  let startWorkerAction (Just worker) workplace = Just $ \player -> startWorking player worker workplace
      startWorkerAction _ _ = Nothing
  tellPlayerAction $ attachWithMaybe startWorkerAction (current selectedWorker) workplaceClicks

type SelectedPlayerWithSettingsChanges t = (Dynamic t PlayerId, Event t SinglePlayerSettings)

drawPlayerSelection :: UniverseWidget t m x => m (Dynamic t PlayerId)
drawPlayerSelection =
  divAttributeLike playerContainerClass $ do
    universeDyn <- askUniverseDyn
    rec
      let players = getPlayers <$> universeDyn
      listOfEvents <- simpleListOrd players $ (drawPlayerInSelection selectedPlayer)
      let playerClicks = leftmost <$> listOfEvents
      selectedPlayer <- findSelectedPlayer universeDyn playerClicks
    holdUniqDyn selectedPlayer

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
  holdUniqDyn result

drawPlayerInSelection :: UniverseWidget t m x => Dynamic t PlayerId -> PlayerId -> m (Event t PlayerId)
drawPlayerInSelection selectedPlayerId playerId  = do
  universeDyn <- askUniverseDyn
  settingsDyn <- askPlayerSettings
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

drawPlayerInfo :: PlayerWidget t m x => m ()
drawPlayerInfo = do
  divAttributeLike playerInfoClass $ do
    drawPlayerResources
    drawFinishButton
    drawCollectButton
    drawHireButton
    drawArming
    drawAdventures

drawPlayerResources :: PlayerWidget t m x => m ()
drawPlayerResources = do
  resources <- askResources
  drawResourcesDyn resources

drawFinishButton :: PlayerWidget t m x => m ()
drawFinishButton = drawActionButton canFinishAction finishAction "Finish action"

drawCollectButton :: PlayerWidget t m x => m ()
drawCollectButton = drawActionButton canCollectResources collectResources "Collect resources"

drawHireButton :: PlayerWidget t m x => m ()
drawHireButton = drawActionButton canHireWorker hireWorker "Hire worker"

drawArming :: PlayerWidget t m x => m ()
drawArming = do
  universeDyn <- askUniverseDyn
  plId <- askPlayerId
  whenWidget (isArmingWorker <$> universeDyn <*> pure plId) $ divAttributeLike armingClass $ do
    strengthText <- textInput (def & textInputConfig_inputType .~ "number" & textInputConfig_initialValue .~ "1")
    clicks <- button "Arm"
    let strength = readMaybe <$> T.unpack <$> current (value strengthText)
        event = fmapMaybe id $ fmap (flip armWorker) <$> tag strength clicks
    tellPlayerAction event

drawAdventures :: PlayerWidget t m x => m()
drawAdventures = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  whenWidget (canGoOnAdventure <$> universeDyn <*> pure playerId) $ do
    forM_ [WoodReward, GrassReward, SmallPastureReward, SheepReward, LargePastureReward, BarnReward] $ \reward -> do
      let label = case reward of
            WoodReward -> "Collect wood"
            GrassReward -> "Clear forest tile"
            SmallPastureReward -> "Build a small pasture"
            SheepReward -> "Get sheep"
            LargePastureReward -> "Build a large pasture"
            BarnReward -> "Build a barn"
      event <- button label
      tellPlayerAction $ (const (flip adventure reward)) <$> event

drawActionButton :: PlayerWidget t m x => (Universe -> PlayerId -> Bool) -> (PlayerId -> Universe -> Either String Universe) -> T.Text -> m ()
drawActionButton condition action label = do
  universeDyn <- askUniverseDyn
  playerId <- askPlayerId
  whenWidget (condition <$> universeDyn <*> pure playerId) $ do
    event <- button label
    tellPlayerAction $ const action <$> event

askResources :: PlayerWidget t m x => m (Dynamic t Resources)
askResources = do
  u <- askUniverseDyn
  player <- askPlayerId
  return $ getPlayerResources <$> u <*> constDyn player


