{-# LANGUAGE FlexibleContexts #-}
module Player.Worker.Dom where

import Rules
import Common.DomUtil
import Common.CommonClasses
import Player.Worker.Style
import Settings.Types
import Player.Types

import Reflex.Dom
import Data.Monoid

drawWorker :: PlayerWidget t m => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  colorDyn <- getWorkerColor
  let addHighlight color selectedWorker = if selectedWorker == Just workerId then colorClass color <> activeWorkerClass <> workerAnimationClass else colorClass color <> workerAnimationClass
      mainClass = addHighlight <$> colorDyn <*> selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) animationStates $ return ()
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  return $ const workerId <$> filteredClicks

getWorkerColor :: PlayerWidget t m => m (Dynamic t PlayerColor)
getWorkerColor = do
  settings <- Player.Types.askPlayerSettings
  return $ playerColor <$> settings
