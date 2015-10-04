{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

import           Control.Monad
import           CssClass
import           Reflex
import           Reflex.Dom
import           Style
import           Data.List
import           Data.Maybe
import           Data.Map as M
import           Rules
import           ReflexUtil

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
  rec
    score <- mapDyn getScore universe
    drawScore score
    drawFreeWorkers universe
    drawWorkplaces universe
    universe <- foldDyn (\x y -> y) initialUniverse never
  return ()

freeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t [WorkerId])
freeWorkers universe = do
  let getFreeWorkers un = [w | w <- getWorkers un, isNothing $ getWorkerWorkplace un w]
  mapDyn getFreeWorkers universe

drawScore :: MonadWidget t m => Dynamic t Int -> m ()
drawScore score = do
  scoreString <- mapDyn show score
  divCssClass scoreClass $ dynText scoreString
  return ()

drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Event t WorkerId)
drawFreeWorkers universe = do
  (_, ev) <- divCssClass freeWorkersClass $ do
    free <- freeWorkers universe
    let combineWorkerClicks :: MonadWidget t m => [WorkerId] -> m (Event t WorkerId)
        combineWorkerClicks workers = let events = mapM drawWorker workers
                                      in leftmost <$> events
    combineWorkerClicks `mapDyn` free >>= dynEvent
  return ev

drawWorker :: MonadWidget t m => WorkerId -> m (Event t WorkerId)
drawWorker workerId = do
  (divEl, _) <- divCssClass workerClass $ return ()
  let clicks = domEvent Click divEl
  return $ const workerId <$> clicks

drawWorkplaces :: MonadWidget t m => Dynamic t Universe -> m (Event t WorkplaceId)
drawWorkplaces universe = do
  let drawWorkplace :: MonadWidget t m => WorkplaceId -> [WorkerId] -> m (Event t WorkplaceId)
      drawWorkplace workplace workers = do
        (el, _) <- divCssClass cardWrapperClass $
          divCssClass cardClass $
            mapM_ drawWorker workers
        return $ const workplace <$> domEvent Click el
      drawWorkplacesInUniverse universe =
              let workplaces = M.keys $ getWorkplaces universe
                  findWorkplaceWorkers universe workplace = (workplace, getWorkplaceOccupants universe workplace)
                  workplacesWithWorkers = findWorkplaceWorkers universe <$> workplaces
                  events = mapM (uncurry drawWorkplace) workplacesWithWorkers
              in leftmost <$> events
  dynamic <- drawWorkplacesInUniverse `mapDyn` universe
  dynEvent dynamic
