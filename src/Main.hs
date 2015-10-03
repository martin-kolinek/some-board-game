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

drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> m ()
drawFreeWorkers universe = do
  divCssClass freeWorkersClass $ do
    free <- freeWorkers universe
    dynamic <- mapM_ drawWorker `mapDyn` free
    dyn dynamic
    return ()
  return ()

drawWorker :: MonadWidget t m => WorkerId -> m ()
drawWorker workerId = void (divCssClass workerClass $ return ())

drawWorkplaces :: MonadWidget t m => Dynamic t Universe -> m ()
drawWorkplaces universe = do
  let drawWorkplace :: MonadWidget t m => WorkplaceId -> [WorkerId] -> m ()
      drawWorkplace workplace workers = void $
        divCssClass cardWrapperClass $
          divCssClass cardClass $
            mapM_ drawWorker workers
      drawWorkplacesInUniverse universe =
              let workplaces = M.keys $ getWorkplaces universe
                  findWorkplaceWorkers universe workplace = (workplace, getWorkplaceOccupants universe workplace)
                  workplacesWithWorkers = findWorkplaceWorkers universe <$> workplaces
              in mapM_ (uncurry drawWorkplace) workplacesWithWorkers
  dynamic <- drawWorkplacesInUniverse `mapDyn` universe
  dyn dynamic
  return ()
