{-# LANGUAGE FlexibleContexts #-}
module Player.Worker.Dom where

import Rules
import Common.DomUtil
import Player.Worker.Style
import Settings.Types
import Player.Types

import Reflex.Dom
import Data.Monoid

drawWorker :: PlayerWidget t m => Dynamic t (Maybe WorkerId) -> Dynamic t WorkerId -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerIdDyn = do
  colorDyn <- getWorkerColor
  let addHighlight color selectedWorker workerId = if selectedWorker == Just workerId then colorClass color <> activeWorkerClass <> workerAnimationClass else colorClass color <> workerAnimationClass
      mainClass = addHighlight <$> colorDyn <*> selectedWorkerDyn <*> workerIdDyn
  (divEl, _) <- divAttributeLikeDyn' mainClass $ return ()
  let clicks = domEvent Click divEl
  return $ tag (current workerIdDyn) clicks

getWorkerColor :: PlayerWidget t m => m (Dynamic t PlayerColor)
getWorkerColor = do
  settings <- Player.Types.askPlayerSettings
  return $ playerColor <$> settings
