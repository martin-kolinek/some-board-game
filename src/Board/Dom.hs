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

drawBoard :: (UniverseReader t m, MonadWidget t m) => m (Event t UniverseAction)
drawBoard = do
  universe <- askUniverse
  rec
    let isFree worker universe = isNothing $ getWorkerWorkplace universe =<< worker
        deselects = ffilter not $ attachWith isFree (current selectedWorker) (updated universe)
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
  universe <- askUniverse
  workplaces <- getWorkplaces `mapDyn` universe
  let drawWorkplace workplaceId workplaceAction = do
        workersInWorkplace <- forDyn universe (`getWorkplaceOccupants` workplaceId)
        (el, _) <- divCssClass cardWrapperClass $
          divCssClass cardClass $
            animatedList (fromRational 1) workersInWorkplace (drawWorker $ constDyn Nothing)
        return $ const workplaceId <$> domEvent Click el
  events <- listWithKey workplaces drawWorkplace
  let combineEvents map = leftmost (M.elems map)
  event <- combineEvents `mapDyn` events
  return $ switch (current event)
