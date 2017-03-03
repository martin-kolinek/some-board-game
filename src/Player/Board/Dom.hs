{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses  #-}

module Player.Board.Dom where

import Rules
import Types
import Common.DomUtil
import Player.Dom
import Player.Types
import Player.Worker.Dom
import Settings.Dom
import Settings.Types
import Player.Board.Style

import Reflex.Dom
import Data.Map.Strict as M hiding (map)
import Control.Monad
import Control.Monad.Reader
import Prelude hiding (map)
import Data.Maybe (fromJust)

drawBoard :: (UniverseReader t m x, MonadWidget t m) => m (Event t UniverseAction)
drawBoard = do
  universeDyn <- askUniverse
  rec
    (result, settings) <- flip runReaderT (universeDyn, settings) $ do
      innerSettings <- drawSettingsIcon universeDyn
      playerExports <- drawPlayers
      workplaceClicks <- drawWorkplaces
      let selectedWorker = extractSelectedWorker playerExports
          selectedPlayer = extractSelectedPlayer playerExports
          selectPositions = attachWith (\a (b, c, d) -> buildBuildings a b c d) (fromJust <$> current selectedPlayer) (extractPositionSelections playerExports)
          startWorkingActions = createWorkAssignments (current universeDyn) (current selectedPlayer) (current selectedWorker) workplaceClicks
          changeOccupantsActions = uncurry alterOccupants <$> extractOccupantChanges playerExports
      return (leftmost [startWorkingActions, changeOccupantsActions, selectPositions], innerSettings)
  return result

createWorkAssignments :: Reflex t => Behavior t Universe -> Behavior t (Maybe PlayerId) -> Behavior t (Maybe WorkerId) -> Event t WorkplaceId -> Event t UniverseAction
createWorkAssignments universeBehavior playerBehavior selectedWorkerBehavior workplaceClicks =
  let workerPlayers universe selectedWorker = [player | player <- getPlayers universe, worker <- getWorkers universe player, selectedWorker == worker]
      playerHasOccupantErrors universe selectedWorker = any (not . Prelude.null . getOccupantErrors universe) (workerPlayers universe selectedWorker)
      combineFunc (universe, Just playerId, Just selectedWorker) workplace =
        Just $ if playerHasOccupantErrors universe selectedWorker then const (Left "Fix occupants") else startWorking playerId selectedWorker workplace
      combineFunc _ _ = Nothing
      in attachWithMaybe combineFunc ((,,) <$> universeBehavior <*> playerBehavior <*> selectedWorkerBehavior) workplaceClicks

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
cardContents workplaceData = text $ show $ getWorkplaceResources workplaceData
