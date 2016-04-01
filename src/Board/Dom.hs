{-# LANGUAGE RecursiveDo, FlexibleContexts  #-}

module Board.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Player.Dom
import Board.Worker.Dom
import Board.Style

import Reflex.Dom
import Data.Maybe
import Data.Map.Strict as M
import Control.Monad

drawBoard :: (UniverseReader t m, MonadWidget t m) => m (Event t UniverseAction)
drawBoard = do
  universeDyn <- askUniverse
  rec
    let isFree worker universe = isNothing $ getWorkerWorkplace universe =<< worker
        deselects = ffilter not $ attachWith isFree (current selectedWorker) (updated universeDyn)
    currentPlayer <- drawPlayerSelection
    freeWorkersDrawn <- mapDyn (drawFreeWorkers selectedWorker) currentPlayer
    workerClicksSwitches <- dyn freeWorkersDrawn
    workerClicksBehavior <- hold never workerClicksSwitches
    let workerClicks = switch workerClicksBehavior
    workplaceClicks <- drawWorkplaces
    selectedWorker <- holdDyn Nothing $ leftmost [Just <$> workerClicks, const Nothing <$> deselects]
    let workplaceClicksWithSelectedWorker = attach (current selectedWorker) workplaceClicks
        extractAssignWork (Just worker, workplace) = Just (worker, workplace)
        extractAssignWork _ = Nothing
        workAssignemnts = fmapMaybe extractAssignWork workplaceClicksWithSelectedWorker
  ev <- getPostBuild
  return $ uncurry startWorking <$> workAssignemnts

drawWorkplaces :: (UniverseReader t m, MonadWidget t m) => m (Event t WorkplaceId)
drawWorkplaces = do
  workplaces <- askWorkplaces
  let drawWorkplace workplaceId workplaceAction = do
        workersInWorkplace <- askWorkplaceOccupants workplaceId
        (el, _) <- divCssClass cardWrapperClass $
          divCssClass cardClass $
            animatedList (fromRational 1) workersInWorkplace (drawWorker $ constDyn Nothing)
        return $ const workplaceId <$> domEvent Click el
  events <- listWithKey workplaces drawWorkplace
  let combineEvents map = leftmost (M.elems map)
  event <- combineEvents `mapDyn` events
  return $ switch (current event)

askWorkplaces :: (UniverseReader t m, MonadWidget t m) => m (Dynamic t (Map WorkplaceId WorkplaceAction))
askWorkplaces = join $ mapDyn getWorkplaces <$> askUniverse

askWorkplaceOccupants :: (UniverseReader t m, MonadWidget t m) => WorkplaceId -> m (Dynamic t [WorkerId])
askWorkplaceOccupants workplaceId = join $ mapDyn (flip getWorkplaceOccupants workplaceId) <$> askUniverse
