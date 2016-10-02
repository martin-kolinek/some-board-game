{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses  #-}

module Board.Dom where

import Rules
import Types
import Common.DomUtil
import Board.Player.Dom
import Board.Player.Types
import Board.Worker.Dom
import Board.Settings.Dom
import Board.Settings.Types
import Board.Style

import Reflex.Dom
import Data.Map.Strict as M hiding (map)
import Control.Monad
import Control.Monad.Reader
import Prelude hiding (map)

drawBoard :: (UniverseReader t m x, MonadWidget t m) => m (Event t UniverseAction)
drawBoard = do
  universeDyn <- askUniverse
  rec
    (result, settings) <- flip runReaderT (universeDyn, settings) $ do
      innerSettings <- drawSettingsIcon
      playerExports <- drawPlayers
      workplaceClicks <- drawWorkplaces
      let selectedWorker = extractSelectedWorker playerExports
          startWorkingActions = createWorkAssignments (current universeDyn) (current selectedWorker) workplaceClicks
          changeOccupantsActions = uncurry alterOccupants <$> extractOccupantChanges playerExports
          selectPositions = uncurry selectPosition <$> extractPositionSelections playerExports
          cancelActions = const cancelSelection <$> extractCancels playerExports
      return (leftmost [startWorkingActions, changeOccupantsActions, selectPositions, cancelActions], innerSettings)
  return result

createWorkAssignments :: Reflex t => Behavior t Universe -> Behavior t (Maybe WorkerId) -> Event t WorkplaceId -> Event t UniverseAction
createWorkAssignments universeBehavior selectedWorkerBehavior workplaceClicks =
  let workerPlayers universe selectedWorker = [player | player <- getPlayers universe, worker <- getWorkers universe player, selectedWorker == worker]
      playerHasOccupantErrors universe selectedWorker = any (not . Prelude.null . getOccupantErrors universe) (workerPlayers universe selectedWorker)
      combineFunc (universe, Just selectedWorker) workplace =
        Just $ if playerHasOccupantErrors universe selectedWorker then const (Left "Fix occupants") else startWorking selectedWorker workplace
      combineFunc _ _ = Nothing
      in attachWithMaybe combineFunc ((,) <$> universeBehavior <*> selectedWorkerBehavior) workplaceClicks

drawWorkplaces :: (PlayerSettingsReader t m x, UniverseReader t m x, MonadWidget t m) => m (Event t WorkplaceId)
drawWorkplaces =
  divAttributeLike workplacesClass $ do
    workplaces <- askWorkplaces
    let drawWorkplace workplaceId dataDyn = do
          workersInWorkplace <- askWorkplaceOccupants workplaceId
          attributesDyn <- mapDyn cardCss dataDyn
          (element, _) <- divAttributeLike' cardWrapperClass $
            divAttributeLikeDyn' attributesDyn $ do
              mapDynExtract cardContents dataDyn
              animatedList (fromRational 1) workersInWorkplace (drawWorker $ constDyn Nothing)
          return $ const workplaceId <$> domEvent Click element
    events <- listWithKey workplaces drawWorkplace
    let combineEvents map = leftmost (M.elems map)
    event <- combineEvents `mapDyn` events
    return $ switch (current event)

askWorkplaces :: (UniverseReader t m x, MonadWidget t m) => m (Dynamic t (Map WorkplaceId WorkplaceData))
askWorkplaces = join $ mapDyn getWorkplaces <$> askUniverse

askWorkplaceOccupants :: (UniverseReader t m x, MonadWidget t m) => WorkplaceId -> m (Dynamic t [WorkerId])
askWorkplaceOccupants workplaceId = join $ mapDyn (flip getWorkplaceOccupants workplaceId) <$> askUniverse

cardContents :: MonadWidget t m => WorkplaceData -> m ()
cardContents (CutForest n) = text ("Wood: " ++ show n)
cardContents (DigCave n) = text ("Stone: " ++ show n)
cardContents (DigPassage n) = text ("Stone: " ++ show n)
cardContents ChildDesire = return ()
