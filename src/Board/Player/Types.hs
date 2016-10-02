{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Board.Player.Types where

import Rules
import Common.DomUtil

import Reflex.Dom

data PlayerExports t = PlayerExports {
  extractSelectedWorker :: Dynamic t (Maybe WorkerId),
  extractOccupantChanges :: Event t (PlayerId, BuildingOccupants),
  extractPositionSelections :: Event t (Position, Direction),
  extractCancels :: Event t (),
  extractChoseOption :: Event t ChildDesireOptions
}

instance MonadWidget t m => ExtractableFromEvent t m (PlayerExports t) where
  extractFromEvent ev =
    let toTuple (PlayerExports a b c d e) = ((((a, b), c), d), e)
        fromTuple ((((a, b), c), d), e) = PlayerExports a b c d e
        tupleEvent = toTuple <$> ev
        extracted = extractFromEvent tupleEvent
    in fromTuple <$> extracted
