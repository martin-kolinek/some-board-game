{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad
import           CssClass
import           Data.List
import           Data.Map.Strict as M
import           Data.Maybe
import           Reflex
import           Reflex.Dom
import           ReflexUtil
import           Rules
import           Style

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
  -- rec
  --   score <- mapDyn getScore universe
  --   scoreActions <- drawScore universe
  --   boardActions <- drawBoard universe
  --   let actions = leftmost [boardActions, scoreActions]
  --   let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
  --   drawErrors universe actions
  --   universe <- foldDyn tryApplyToUniverse initialUniverse actions
  -- return ()
  (el, _) <- elAttr' "div" ("class" =: "test2") $ text "Hello"
  divAnimFromWithClass testClass el $ text "Hello anim"
  return ()

-- type UniverseAction = Universe -> Either String Universe
--
-- drawErrors :: MonadWidget t m => Dynamic t Universe -> Event t UniverseAction -> m ()
-- drawErrors universe actions = void $ divCssClass errorContainerClass $ do
--   let extractError (un, act) = fromLeft $ act un
--       attached = attach (current universe) actions
--       errorEvents = fmapMaybe extractError attached
--       addToMap :: String -> Map Int String -> Map Int String
--       addToMap err map = if M.null map then singleton 1 err else M.insert newIndex err map
--         where newIndex = fst (findMax map) + 1
--       attachIdToEvent :: Reflex t => (Int, Event t ()) -> Event t Int
--       attachIdToEvent (id, event) = const id <$> event
--       extractEventsFromMap map = leftmost $ attachIdToEvent <$> assocs map
--       drawError :: Reflex t => MonadWidget t m => Dynamic t String -> m (Event t ())
--       drawError err = divClass "" $ dynText err >> button "Close"
--   rec
--     closeEvents <- list allErrors drawError
--     allErrors <- foldDyn modifyMap empty addAndRemoveEvents
--     let closeKeyEvents = switch $ extractEventsFromMap <$> current closeEvents
--         modifyMap (Left id) = M.delete id
--         modifyMap (Right err) = addToMap err
--         addAndRemoveEvents = leftmost [Right <$> errorEvents, Left <$> closeKeyEvents]
--   return ()
--
-- drawBoard :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
-- drawBoard universe = do
--   rec
--     let deselects = const Nothing <$> leftmost [workAssignemnts]
--     workerClicks <- drawFreeWorkers universe
--     workplaceClicks <- drawWorkplaces universe
--     selectedWorker <- holdDyn Nothing $ leftmost [Just <$> extractFreeWorkerDataEvent workerClicks, deselects]
--     let workplaceClicksWithSelectedWorker = attach (current selectedWorker) workplaceClicks
--         extractAssignWork (Just worker, workplace) = Just (worker, workplace)
--         extractAssignWork _ = Nothing
--         workAssignemnts = fmapMaybe extractAssignWork workplaceClicksWithSelectedWorker
--   return $ uncurry startWorking <$> workAssignemnts
--
-- freeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t [WorkerId])
-- freeWorkers universe = do
--   let getFreeWorkers un = [w | w <- getWorkers un, isNothing $ getWorkerWorkplace un w]
--   mapDyn getFreeWorkers universe
--
-- drawScore :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
-- drawScore score = do
--   scoreString <- mapDyn (show . getScore) score
--   (_, event) <- divCssClass scoreClass $ do
--     finishTurnEvent <- button "Finish turn"
--     dynText scoreString
--     return finishTurnEvent
--   return $ const finishTurn <$> event
--
-- data FreeWorkerData t = FreeWorkerData (Event t WorkerId) (Map WorkerId (El t))
--
-- freeWorkerData wId event el = FreeWorkerData (const wId <$> event) (singleton wId el)
-- getFreeWorkerDataEvent (FreeWorkerData ev _) = ev
--
-- extractFreeWorkerDataEvent :: Reflex t => Dynamic t (FreeWorkerData t) -> Event t WorkerId
-- extractFreeWorkerDataEvent dynamic = switch $ getFreeWorkerDataEvent <$> current dynamic
--
-- instance Reflex t => Monoid (FreeWorkerData t) where
--   mempty = FreeWorkerData never M.empty
--   mappend (FreeWorkerData e1 m1) (FreeWorkerData e2 m2) = FreeWorkerData (leftmost [e1, e2]) (M.union m1 m2)
--
-- drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t (FreeWorkerData t))
-- drawFreeWorkers universe = do
--   (_, ev) <- divCssClass freeWorkersClass $ do
--     free <- freeWorkers universe
--     let combineWorkerClicks :: MonadWidget t m => [WorkerId] -> m (FreeWorkerData t)
--         combineWorkerClicks workers = let events = mapM (drawWorker mempty) workers
--                                       in mconcat <$> events
--     combineWorkerClicks `mapDyn` free >>= dynHold
--   return ev
--
-- drawWorker :: MonadWidget t m => FreeWorkerData t -> WorkerId -> m (FreeWorkerData t)
-- drawWorker workerData workerId = do
--   (divEl, _) <- divCssClass workerClass $ return ()
--   let clicks = domEvent Click divEl
--   return $ freeWorkerData workerId clicks divEl
--
-- drawWorkplaces :: MonadWidget t m => Dynamic t Universe -> Dynamic t (FreeWorkerData t) -> m (Event t WorkplaceId)
-- drawWorkplaces universe workerData = do
--   let drawWorkplace :: MonadWidget t m => FreeWorkerData t -> WorkplaceId -> [WorkerId] -> m (Event t WorkplaceId)
--       drawWorkplace workerData workplace workers = do
--         (el, _) <- divCssClass cardWrapperClass $
--           divCssClass cardClass $
--             mapM_ (drawWorker workerData) workers
--         return $ const workplace <$> domEvent Click el
--       drawWorkplacesInUniverse universe workerData =
--               let workplaces = M.keys $ getWorkplaces universe
--                   findWorkplaceWorkers universe workplace = (workplace, getWorkplaceOccupants universe workplace)
--                   workplacesWithWorkers = findWorkplaceWorkers universe <$> workplaces
--                   events = mapM (uncurry $ drawWorkplace workerData) workplacesWithWorkers
--               in leftmost <$> events
--   dynamic <- combineDyn drawWorkplacesInUniverse universe workerData
--   dynEvent id dynamic
