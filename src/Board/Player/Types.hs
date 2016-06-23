{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Board.Player.Types where

import Rules
import Common.DomUtil

import Reflex.Dom
import Control.Monad.Reader
import Data.Maybe

data PlayerExports t = PlayerExports {
  extractSelectedWorker :: Dynamic t (Maybe WorkerId),
  extractOccupantChanges :: Event t (PlayerId, BuildingOccupants),
  extractPositionSelections :: Event t Position
}

instance MonadWidget t m => ExtractableFromEvent t m (PlayerExports t) where
  extractFromEvent ev =
    let toTuple (PlayerExports a b c) = ((a, b), c)
        fromTuple ((a, b), c) = PlayerExports a b c
        tupleEvent = toTuple <$> ev
        extracted = extractFromEvent tupleEvent
    in fromTuple <$> extracted
