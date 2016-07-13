module Board.Worker.Dom where

import Rules
import Common.DomUtil
import Common.CommonClasses
import Board.Worker.Style
import Types
import Board.Settings.Types

import Reflex.Dom
import Data.Monoid
import Data.Map.Strict as M

drawWorker :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Dynamic t (Maybe WorkerId) -> WorkerId -> Dynamic t AnimationState -> m (Event t WorkerId)
drawWorker selectedWorkerDyn workerId animationStates = do
  colorDyn <- getWorkerColor workerId
  let addHighlight color selectedWorker = if selectedWorker == Just workerId then colorClass color <> activeWorkerClass <> workerAnimationClass else colorClass color <> workerAnimationClass
  mainClass <- combineDyn addHighlight colorDyn selectedWorkerDyn
  (divEl, _) <- animateState mainClass (constDyn fadeClass) animationStates $ return ()
  let clicks = domEvent Click divEl
      filteredClicks = filterByBehavior (/=Fading) (current animationStates) clicks
  postBuild <- getPostBuild
  return $ const workerId <$> filteredClicks

getWorkerColor :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => WorkerId -> m (Dynamic t PlayerColor)
getWorkerColor workerId = do
  universe <- askUniverse
  let getPlayer universe = M.lookup workerId playerMap
          where playerMap = M.fromList [(worker, player) | player <- getPlayers universe, worker <- getWorkers universe player]
      getPlayerColor playerSettings (Just playerId) = playerColor $ singlePlayerSettings playerSettings playerId
      getPlayerColor _ _ = PlayerGreen
  player <- mapDyn getPlayer universe
  settings <- askPlayerSettings
  combineDyn getPlayerColor settings player
