{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Player.Types where

import Rules
import Common.DomUtil
import Settings.Types

import Reflex.Dom
import Control.Monad.Reader

data PlayerExports t = PlayerExports {
  extractSelectedPlayer :: Dynamic t (Maybe PlayerId),
  extractSelectedWorker :: Dynamic t (Maybe WorkerId),
  extractOccupantChanges :: Event t (PlayerId, BuildingOccupants),
  extractPositionSelections :: Event t (Position, Direction, [BuildingType]),
  extractFinishAction :: Event t ()
}

instance MonadWidget t m => ExtractableFromEvent t m (PlayerExports t) where
  extractFromEvent ev =
    let toTuple (PlayerExports a b c d e) = ((((a, b), c), d), e)
        fromTuple ((((a, b), c), d), e) = PlayerExports a b c d e
        tupleEvent = toTuple <$> ev
        extracted = extractFromEvent tupleEvent
    in fromTuple <$> extracted

data PlayerWidgetData t = PlayerWidgetData {
    playerWidgetUniverse :: Dynamic t Universe,
    playerWidgetPlayerId :: PlayerId,
    playerWidgetSettings :: Dynamic t SinglePlayerSettings
  }

type PlayerWidget t m = (MonadWidget t m, MonadReader (PlayerWidgetData t) m)

askUniverseDyn :: PlayerWidget t m => m (Dynamic t Universe)
askUniverseDyn = asks playerWidgetUniverse

askPlayerId :: PlayerWidget t m => m PlayerId
askPlayerId = asks playerWidgetPlayerId

askPlayerSettings :: PlayerWidget t m => m (Dynamic t SinglePlayerSettings)
askPlayerSettings = asks playerWidgetSettings

type PlayerAction = PlayerId -> Universe -> Either String Universe
