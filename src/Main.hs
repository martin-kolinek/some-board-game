
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections #-}
import           Control.Monad
import           CssClass
import           Reflex
import           Reflex.Dom
import           Style
import           Data.List
import           Data.Maybe
import Data.Monoid
import           Data.Map.Strict as M
import           Rules
import           ReflexUtil
import Data.Time.Clock
import Debug.Trace (traceShowId)

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      currentPlayer <- drawPlayerSelection universe
      scoreActions <- drawScore universe currentPlayer
      boardDrawn <- mapDyn (drawBoard universe) (nubDyn currentPlayer)
      boardActionsDyn <- dynHold boardDrawn
      let boardActions = switch (current boardActionsDyn)
      let actions = leftmost [boardActions, scoreActions]
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
      drawErrors universe actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()

type UniverseAction = Universe -> Either String Universe

drawPlayerSelection :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t PlayerId)
drawPlayerSelection universeDyn = do
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      currentClass isCurrent = if isCurrent then currentPlayerClass else mempty
      drawPlayer selectedPlayerId playerId  = do
        playerString <- mapDyn show playerId
        isSelected <- combineDyn (==) playerId selectedPlayerId
        selectedClassDyn <- mapDyn selectedClass isSelected
        currentPlayerDyn <- mapDyn getCurrentPlayer universeDyn
        isCurrent <- combineDyn ((==) . Just) playerId currentPlayerDyn
        currentClassDyn <- mapDyn currentClass isCurrent
        classDyn <- mconcatDyn [constDyn playerClass, currentClassDyn, selectedClassDyn]
        (el, _) <- divCssClassDyn classDyn $ dynText playerString
        let event = domEvent Click el
        return $ tag (current playerId) event
  rec
    (_, selectedPlayer) <- divCssClass playerContainerClass $ do
      players <- mapDyn getPlayers universeDyn
      events <- simpleList players (drawPlayer selectedPlayer)
      combined <- mapDyn leftmost events
      let selections = switch (current combined)
      maybePlayerId <- holdDyn Nothing (Just <$> selections)
      defaultPlayer <- mapDyn head players
      combineDyn fromMaybe defaultPlayer maybePlayerId
  return selectedPlayer

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
        (_, res) <- animateState (constDyn errorItemClass) (constDyn fadeClass) (constDyn appearClass) animState $ do
          el "div" $ dynText err
          el "div" $ buttonSpanCssClass closeButtonClass (return ())
        return res
      combineWithLast newValue (oldValue, _) = (newValue, newValue M.\\ oldValue)
  rec
    animated <- animateMap (fromRational 1) allErrors
    closeEvents <- listWithKey animated drawError
    allErrors <- foldDyn modifyMap empty addAndRemoveEvents
    additionsDyn <- foldDyn combineWithLast (M.empty, M.empty) (updated allErrors)
    let additions = (M.keys . snd) <$> updated additionsDyn
    timedRemovals <- delay (fromRational 3) additions
    let closeKeyEvents = switch $ extractEventsFromMap <$> current closeEvents
        modifyMap (Left ids) mp = Data.List.foldl' (flip M.delete) mp ids
        modifyMap (Right err) mp = addToMap err mp
        allRemovals = appendEvents (return <$> closeKeyEvents) timedRemovals
        addAndRemoveEvents = leftmost [Right <$> errorEvents, Left <$> allRemovals]

  return ()

drawBoard :: MonadWidget t m => Dynamic t Universe -> PlayerId -> m (Event t UniverseAction)
drawBoard universe player = do
  rec
    let isFree worker universe = isNothing $ getWorkerWorkplace universe =<< worker
    let deselects = ffilter not $ attachWith isFree (current selectedWorker) (updated universe)
    workerClicks <- drawFreeWorkers universe player selectedWorker
    workplaceClicks <- drawWorkplaces universe
    selectedWorker <- holdDyn Nothing $ leftmost [Just <$> workerClicks, const Nothing <$> deselects]
    let workplaceClicksWithSelectedWorker = attach (current selectedWorker) workplaceClicks
        extractAssignWork (Just worker, workplace) = Just (worker, workplace)
        extractAssignWork _ = Nothing
        workAssignemnts = fmapMaybe extractAssignWork workplaceClicksWithSelectedWorker
  return $ uncurry startWorking <$> workAssignemnts

freeWorkers :: MonadWidget t m => Dynamic t Universe -> PlayerId -> m (Dynamic t [WorkerId])
freeWorkers universeDyn player = do
  let getFreeWorkers universe player = [w | w <- getWorkers universe player, isNothing $ getWorkerWorkplace universe w]
  mapDyn (flip getFreeWorkers player) universeDyn

drawScore :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerId -> m (Event t UniverseAction)
drawScore universeDyn playerDyn = do
  score <- combineDyn getScore universeDyn playerDyn
  scoreString <- mapDyn show score
  (_, event) <- divCssClass scoreClass $ do
    finishTurnEvent <- button "Finish turn"
    dynText scoreString
    return finishTurnEvent
  return $ const finishTurn <$> event

drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> PlayerId -> Dynamic t (Maybe WorkerId) -> m (Event t WorkerId)
drawFreeWorkers universeDyn playerDyn selectedWorker = do
  (_, ev) <- divCssClass freeWorkersClass $ do
    free <- freeWorkers universeDyn playerDyn
    animated <- animateList (fromRational 1) free
    events <- listWithKey animated (drawWorker selectedWorker)
    let combineWorkerClicks :: Reflex t => Map WorkerId (Event t WorkerId) -> Event t WorkerId
        combineWorkerClicks workers = leftmost $ elems workers
    combinedClicks <- combineWorkerClicks `mapDyn` events
    return $ switch (current combinedClicks)
  return ev

drawWorker :: MonadWidget t m => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  let addHighlight selectedWorker = if selectedWorker == Just workerId then activeWorkerClass else workerClass
  mainClass <- mapDyn addHighlight selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) (constDyn appearClass) animationStates $ return ()
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
            listWithKey animated (drawWorker $ constDyn Nothing)
        return $ const workplaceId <$> domEvent Click el
  events <- listWithKey workplaces drawWorkplace
  let combineEvents map = leftmost (M.elems map)
  event <- combineEvents `mapDyn` events
  return $ switch (current event)
