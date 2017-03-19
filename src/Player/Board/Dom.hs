{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.Text as T

drawWorkplaces :: PlayerWidget t m => m (Event t WorkplaceId)
drawWorkplaces =
  divAttributeLike workplacesClass $ do
    workplaces <- askWorkplaces
    let drawWorkplace :: PlayerWidget t m => WorkplaceId -> Dynamic t WorkplaceData -> m (Event t WorkplaceId)
        drawWorkplace workplaceId dataDyn = do
          workersInWorkplace <- askWorkplaceOccupants workplaceId
          (wrapperElem, _) <- divAttributeLike' cardWrapperClass $
            divAttributeLikeDyn' (cardCss <$> dataDyn) $ do
              cardContents dataDyn
              animatedList (fromRational 1) workersInWorkplace (drawWorker $ constDyn Nothing)
          return $ const workplaceId <$> domEvent Click wrapperElem
    events <- listWithKey workplaces drawWorkplace
    let combineEvents map = leftmost (M.elems map)
        event = combineEvents <$> events
    return $ switch (current event)

askWorkplaces :: PlayerWidget t m => m (Dynamic t (Map WorkplaceId WorkplaceData))
askWorkplaces = fmap getWorkplaces <$> askUniverseDyn

askWorkplaceOccupants :: PlayerWidget t m => WorkplaceId -> m (Dynamic t [WorkerId])
askWorkplaceOccupants workplaceId = fmap (flip getWorkplaceOccupants workplaceId) <$> askUniverseDyn

cardContents :: MonadWidget t m => Dynamic t WorkplaceData -> m ()
cardContents workplaceData = dynText $ T.pack <$> show <$> getWorkplaceResources <$> workplaceData
