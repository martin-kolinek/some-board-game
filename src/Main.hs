
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
import Control.Monad.IO.Class
import Data.Time.Clock
import Debug.Trace (traceShowId)

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      finishActions <- drawFinish universe
      boardActions <- drawBoard universe
      let actions = leftmost [boardActions, finishActions]
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
      drawErrors universe actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()

type UniverseAction = Universe -> Either String Universe

drawPlayerSelection :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t PlayerId)
drawPlayerSelection universeDyn = do
  currentPlayerDyn <- mapDyn getCurrentPlayer universeDyn
  let selectedClass isSelected = if isSelected then selectedPlayerClass else mempty
      currentClass isCurrent = if isCurrent then currentPlayerClass else mempty
      drawPlayer selectedPlayerId playerId  = do
        playerString <- mapDyn show playerId
        isSelected <- combineDyn (==) playerId selectedPlayerId
        selectedClassDyn <- mapDyn selectedClass isSelected
        isCurrent <- combineDyn ((==) . Just) playerId currentPlayerDyn
        currentClassDyn <- mapDyn currentClass isCurrent
        classDyn <- mconcatDyn [constDyn playerClass, currentClassDyn, selectedClassDyn]
        scoreDyn <- combineDyn getScore universeDyn playerId
        scoreTextDyn <- mapDyn show scoreDyn
        (el, _) <- divCssClassDyn classDyn $ do
          dynText playerString
          dynText scoreTextDyn
        let event = domEvent Click el
        return $ tag (current playerId) event
  rec
    (_, selectedPlayer) <- divCssClass playerContainerClass $ do
      players <- mapDyn getPlayers universeDyn
      events <- simpleList players (drawPlayer selectedPlayer)
      combined <- mapDyn leftmost events
      let userSelections = switch (current combined)
          currentPlayerChangeSelections = fmapMaybe id $ updated currentPlayerDyn
      let selections = leftmost [userSelections, currentPlayerChangeSelections]
      maybePlayerId <- holdDyn Nothing (Just <$> selections)
      defaultPlayer <- mapDyn head players
      combineDyn fromMaybe defaultPlayer maybePlayerId
  return $ nubDyn selectedPlayer

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
      drawError :: Reflex t => MonadWidget t m => Int -> Dynamic t (AnimationState, String) -> m (Event t ())
      drawError key tuple = do
        err <- snd `mapDyn` tuple
        animState <- fst `mapDyn` tuple
        (_, res) <- animateState (constDyn errorItemClass) (constDyn fadeClass) animState $ do
          el "div" $ dynText err
          el "div" $ buttonSpanCssClass closeButtonWithIconClass (return ())
        return res
      combineWithLast newValue (oldValue, _) = (newValue, newValue M.\\ oldValue)
  rec
    closeEvents <- animated (fromRational 1) allErrors drawError
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

drawBoard :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawBoard universe = do
  rec
    let isFree worker universe = isNothing $ getWorkerWorkplace universe =<< worker
        deselects = ffilter not $ attachWith isFree (current selectedWorker) (updated universe)
    currentPlayer <- drawPlayerSelection universe
    freeWorkersDrawn <- mapDyn (drawFreeWorkers universe selectedWorker) currentPlayer
    workerClicksSwitches <- dyn freeWorkersDrawn
    workerClicksBehavior <- hold never workerClicksSwitches
    let workerClicks = switch workerClicksBehavior
    workplaceClicks <- drawWorkplaces universe
    selectedWorker <- holdDyn Nothing $ leftmost [Just <$> workerClicks, const Nothing <$> deselects]
    let workplaceClicksWithSelectedWorker = attach (current selectedWorker) workplaceClicks
        extractAssignWork (Just worker, workplace) = Just (worker, workplace)
        extractAssignWork _ = Nothing
        workAssignemnts = fmapMaybe extractAssignWork workplaceClicksWithSelectedWorker
  ev <- getPostBuild
  return $ uncurry startWorking <$> workAssignemnts

freeWorkers :: MonadWidget t m => Dynamic t Universe -> PlayerId -> m (Dynamic t [WorkerId])
freeWorkers universeDyn player = do
  let getFreeWorkers universe player = [w | w <- getWorkers universe player, isNothing $ getWorkerWorkplace universe w]
  mapDyn (flip getFreeWorkers player) universeDyn

drawFinish :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawFinish universeDyn = do
  (_, event) <- divCssClass scoreClass $ button "Finish turn"
  return $ const finishTurn <$> event

drawFreeWorkers :: MonadWidget t m => Dynamic t Universe -> Dynamic t (Maybe WorkerId) -> PlayerId -> m (Event t WorkerId)
drawFreeWorkers universeDyn selectedWorker player = do
  (_, ev) <- divCssClass freeWorkersClass $ do
    free <- freeWorkers universeDyn player
    events <- animatedList (fromRational 1) free (drawWorker selectedWorker)
    let combineWorkerClicks :: Reflex t => Map WorkerId (Event t WorkerId) -> Event t WorkerId
        combineWorkerClicks workers = leftmost $ elems workers
    combinedClicks <- combineWorkerClicks `mapDyn` events
    return $ switch (current combinedClicks)
  return ev

drawWorker :: MonadWidget t m => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  let addHighlight selectedWorker = if selectedWorker == Just workerId then activeWorkerClass else workerClass
  mainClass <- mapDyn addHighlight selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) animationStates $ return ()
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  postBuild <- getPostBuild
  return $ const workerId <$> filteredClicks

drawWorkplaces :: MonadWidget t m => Dynamic t Universe -> m (Event t WorkplaceId)
drawWorkplaces universe = do
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
