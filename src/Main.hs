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

data Worker = Worker Int deriving Eq

data IdleWorkersResult t = IdleWorkersResult {
  workerSelected :: Event t Worker
}

data WorkPlaceResult t = WorkPlaceResult {
  workerAttached :: Event t Worker
}

idleWorkers :: MonadWidget t m => Dynamic t [Worker] -> m (IdleWorkersResult t)
idleWorkers workers = do
  workerEvents <- renderWorkers `mapDyn` workers
  (_, nestedEvent) <- divCssClass idleWorkerContainerClass $ dyn workerEvents
  event <- hold never nestedEvent
  return $ IdleWorkersResult $ switch event

workPlace :: MonadWidget t m =>  IdleWorkersResult t -> m (WorkPlaceResult t)
workPlace (IdleWorkersResult workerSelected) = do
  lastSelected <- hold Nothing $ Just <$> workerSelected
  rec
    cardClicks <- card redneredWorkers
    let attachedWorkers = fmapMaybe id $ tag lastSelected cardClicks
    workingWorkers <- foldDyn (:) [] attachedWorkers
    redneredWorkers <- renderWorkers `mapDyn` workingWorkers
  return $ WorkPlaceResult attachedWorkers

renderWorker :: MonadWidget t m => Worker -> m (Event t Worker)
renderWorker worker = do
  (workerDiv, _) <- divCssClass workerClass $ return ()
  return $ worker <$ domEvent Click workerDiv

renderWorkers :: MonadWidget t m => [Worker] -> m (Event t Worker)
renderWorkers workers = do
  events <- forM workers renderWorker

  return $ leftmost events
card :: MonadWidget t m => Dynamic t (m b) -> m (Event t ())
card cardContent = do
  rec
    (wrapper, _) <- divCssClass cardWrapperClass $
                      divCssClass cardClass $
                        dyn cardContent
    let cardClicks = domEvent Click wrapper
  return cardClicks
