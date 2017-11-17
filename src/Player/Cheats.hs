{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Player.Cheats where

import Player.Types
import Universe
import Resources
import Player

import Data.Map.Strict
import Data.AdditiveGroup
import Reflex.Dom
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (keyPress)

cheatsHandler :: PlayerWidget t m x => m ()
cheatsHandler = do
  doc <- askDocument
  event <- wrapDomEvent doc (`on` keyPress) getKeyEvent
  tellPlayerAction $ const addResources <$> (ffilter (== (fromEnum 'r')) $ fromIntegral <$> event)

addResources :: PlayerId -> Universe -> Either String Universe
addResources plId u = Right $ u { _players = updatedPlayers }
  where updatedPlayers = adjust updatePlayerData plId $ _players u
        updatePlayerData plData = plData { _playerResources = updatedResources }
          where updatedResources = _playerResources plData ^+^ Resources 10 10 10 10 10 10 10 10
