{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses  #-}

module Player.Board.Dom where

import Rules
import Common.DomUtil
import Player.Types
import Player.Worker.Dom
import Player.Board.Style

import Reflex.Dom
import Data.Map.Strict as M hiding (map)
import Control.Monad
import Prelude hiding (map)

drawWorkplaces :: PlayerWidget t m => m (Event t WorkplaceId)
drawWorkplaces =
  divAttributeLike workplacesClass $ do
    workplaces <- askWorkplaces
    let drawWorkplace :: PlayerWidget t m => WorkplaceId -> Dynamic t WorkplaceData -> m (Event t WorkplaceId)
        drawWorkplace workplaceId dataDyn = do
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

askWorkplaces :: PlayerWidget t m => m (Dynamic t (Map WorkplaceId WorkplaceData))
askWorkplaces = join $ mapDyn getWorkplaces <$> askUniverseDyn

askWorkplaceOccupants :: PlayerWidget t m => WorkplaceId -> m (Dynamic t [WorkerId])
askWorkplaceOccupants workplaceId = join $ mapDyn (flip getWorkplaceOccupants workplaceId) <$> askUniverseDyn

cardContents :: MonadWidget t m => WorkplaceData -> m ()
cardContents workplaceData = text $ show $ getWorkplaceResources workplaceData
