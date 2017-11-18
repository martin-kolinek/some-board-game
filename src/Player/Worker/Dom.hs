{-# LANGUAGE FlexibleContexts #-}
module Player.Worker.Dom where

import Rules
import Common.DomUtil
import Common.CommonClasses
import Player.Worker.Style
import Settings.Types
import Player.Types
import Types
import Data.Maybe

import Reflex.Dom
import Data.Monoid
import Data.Text as T

drawWorker :: PlayerWidget t m x => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  (colorDyn, strengthDyn) <- getWorkerData workerId
  let addHighlight color selectedWorker = if selectedWorker == Just workerId then colorClass color <> activeWorkerClass <> workerAnimationClass else colorClass color <> workerAnimationClass
      mainClass = addHighlight <$> colorDyn <*> selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) animationStates $ drawWorkerStrength strengthDyn
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  return $ const workerId <$> filteredClicks

drawWorkerStrength :: MonadWidget t m => Dynamic t WorkerStrength -> m ()
drawWorkerStrength strengthDyn =
  whenWidget ((/= 0) <$> strengthDyn) $ divAttributeLike strengthClass $ dynText $ T.pack <$> show <$> strengthDyn

getWorkerData :: PlayerWidget t m x => WorkerId -> m (Dynamic t PlayerColor, Dynamic t WorkerStrength)
getWorkerData workerId= do
  settingsDyn <- askPlayerSettings
  universeDyn <- askUniverseDyn
  let getWorkerPlayerId universe = listToMaybe $ [plId | plId <- getPlayers universe, wId <- getWorkers universe plId, wId == workerId]
      playerIdDyn = getWorkerPlayerId <$> universeDyn
      getPlayerColor (Just playerId) settings = playerColor $ singlePlayerSettings settings playerId
      getPlayerColor _ _ = PlayerGreen
      workerColor = getPlayerColor <$> playerIdDyn <*> settingsDyn
      workerStrength = getWorkerStrength <$> universeDyn <*> pure workerId
  return $ (workerColor, workerStrength)
