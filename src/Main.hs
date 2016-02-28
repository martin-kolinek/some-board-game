{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections #-}

import           Control.Monad
import           CssClass
import           Reflex
import           Reflex.Dom
import           Style
import           Data.List
import           Data.Maybe
import           Data.Map.Strict as M
import           Rules
import           ReflexUtil
import Data.Time.Clock
import Debug.Trace (trace)

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      score <- mapDyn getScore universe
      scoreActions <- drawScore universe
      boardActions <- drawBoard universe
      let actions = leftmost [boardActions, scoreActions]
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
      drawErrors universe actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()

type UniverseAction = Universe -> Either String Universe

drawErrors :: MonadWidget t m => Dynamic t Universe -> Event t UniverseAction -> m ()
drawErrors universe actions = void $ divCssClass errorContainerClass $ do
  let extractError (un, act) = fromLeft $ act un
      attached = attach (current universe) actions
      errorEvents = fmapMaybe extractError attached
      addToMap :: String -> Map Int String -> Map Int String
      addToMap err map = if M.null map then singleton 1 err else M.insert newIndex err map
        where newIndex = fst (findMax map) + 1
      attachIdToEvent :: Reflex t => (Int, Event t ()) -> Event t Int
      attachIdToEvent (id, event) = const id <$> event
      extractEventsFromMap map = leftmost $ attachIdToEvent <$> assocs map
      drawError :: Reflex t => MonadWidget t m => Int -> Dynamic t (String, AnimationState) -> m (Event t ())
      drawError key tuple = do
        err <- fst `mapDyn` tuple
        animState <- snd `mapDyn` tuple
        (_, res) <- animateState errorItemClass fadeClass appearClass animState $ do
          el "div" $ dynText err
          el "div" $ buttonSpanCssClass closeButtonClass (return ())
        return res
  rec
    animated <- animateMap (fromRational 1) allErrors
    closeEvents <- listWithKey animated drawError
    allErrors <- foldDyn modifyMap empty addAndRemoveEvents
    let closeKeyEvents = switch $ extractEventsFromMap <$> current closeEvents
        modifyMap (Left id) = M.delete id
        modifyMap (Right err) = addToMap err
        addAndRemoveEvents = leftmost [Right <$> errorEvents, Left <$> closeKeyEvents]
  return ()

drawBoard :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawBoard universe = do
  rec
    let deselects = const Nothing <$> leftmost [workAssignemnts]
    workerClicks <- drawFreeWorkers universe
    workplaceClicks <- drawWorkplaces universe
    selectedWorker <- holdDyn Nothing $ leftmost [Just <$> workerClicks, deselects]
    let workplaceClicksWithSelectedWorker = attach (current selectedWorker) workplaceClicks
        extractAssignWork (Just worker, workplace) = Just (worker, workplace)
        extractAssignWork _ = Nothing
        workAssignemnts = fmapMaybe extractAssignWork workplaceClicksWithSelectedWorker
  return $ uncurry startWorking <$> workAssignemnts

freeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t [WorkerId])
freeWorkers universe = do
  let getFreeWorkers un = [w | w <- getWorkers un, isNothing $ getWorkerWorkplace un w]
  mapDyn getFreeWorkers universe

drawScore :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawScore score = do
  scoreString <- mapDyn (show . getScore) score
  (_, event) <- divCssClass scoreClass $ do
    finishTurnEvent <- button "Finish turn"
    dynText scoreString
    return finishTurnEvent
  return $ const finishTurn <$> event

drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> m (Event t WorkerId)
drawFreeWorkers universe = do
  (_, ev) <- divCssClass freeWorkersClass $ do
    free <- freeWorkers universe
    animated <- animateList (fromRational 1) free
    events <- listWithKey animated drawWorker
    let combineWorkerClicks :: Reflex t => Map WorkerId (Event t WorkerId) -> Event t WorkerId
        combineWorkerClicks workers = leftmost $ elems workers
    combinedClicks <- combineWorkerClicks `mapDyn` events
    return $ switch (current combinedClicks)
  return ev

drawWorker :: MonadWidget t m => WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker workerId animationStates = do
  (divEl, _) <- animateState workerClass fadeClass appearClass animationStates $ return ()
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  return $ const workerId <$> filteredClicks

drawWorkplaces :: MonadWidget t m => Dynamic t Universe -> m (Event t WorkplaceId)
drawWorkplaces universe = do
  workplaces <- getWorkplaces `mapDyn` universe
  let drawWorkplace workplaceId workplaceAction = do
        workersInWorkplace <- forDyn universe (`getWorkplaceOccupants` workplaceId)
        animated <- animateList (fromRational 1) workersInWorkplace
        (el, _) <- divCssClass cardWrapperClass $
          divCssClass cardClass $
            listWithKey animated drawWorker
        return $ const workplaceId <$> domEvent Click el
  events <- listWithKey workplaces drawWorkplace
  let combineEvents map = leftmost (M.elems map)
  event <- combineEvents `mapDyn` events
  return $ switch (current event)
