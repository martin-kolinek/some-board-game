module Board.Worker.Dom where

import Rules
import Common.DomUtil
import Board.Worker.Style

import Reflex.Dom
import Data.Monoid

drawWorker :: MonadWidget t m => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  let addHighlight selectedWorker = if selectedWorker == Just workerId then activeWorkerClass <> workerAnimationClass else workerClass <> workerAnimationClass
  mainClass <- mapDyn addHighlight selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) animationStates $ return ()
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  postBuild <- getPostBuild
  return $ const workerId <$> filteredClicks
