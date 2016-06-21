{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Board.Player.Types where

import Rules
import Common.DomUtil

import Reflex.Dom
import Control.Monad.Reader
import Data.Maybe

data PlayerExports t = PlayerExports { extractSelectedWorker :: Dynamic t (Maybe WorkerId) , extractOccupantChanges :: Event t (PlayerId, BuildingOccupants) }

instance MonadWidget t m => ExtractableFromEvent t m (PlayerExports t) where
  extractFromEvent ev =
    let toTuple (PlayerExports a b) = (a, b)
        fromTuple (a, b) = PlayerExports a b
        tupleEvent = toTuple <$> ev
        extracted = extractFromEvent tupleEvent
    in fromTuple <$> extracted
