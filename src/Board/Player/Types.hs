{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Board.Player.Types where

import Rules
import Common.DomUtil

import Reflex.Dom

data PlayerExports t = PlayerExports {
  extractSelectedWorker :: Dynamic t (Maybe WorkerId),
  extractOccupantChanges :: Event t (PlayerId, BuildingOccupants),
  extractPositionSelections :: Event t (Position, Direction),
  extractCancels :: Event t ()
}

instance MonadWidget t m => ExtractableFromEvent t m (PlayerExports t) where
  extractFromEvent ev =
    let toTuple (PlayerExports a b c d) = (((a, b), c), d)
        fromTuple (((a, b), c), d) = PlayerExports a b c d
        tupleEvent = toTuple <$> ev
        extracted = extractFromEvent tupleEvent
    in fromTuple <$> extracted
