{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Player.Types where

import Rules
import Settings.Types

import Reflex.Dom
import Control.Monad.Reader

data PlayerExports t = PlayerExports {
  extractSelectedWorker :: Dynamic t (Maybe WorkerId),
  extractActions :: Event t PlayerAction
}

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
