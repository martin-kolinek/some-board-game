{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}

module Board.Settings.Types where

import Rules

import Data.Maybe
import Reflex.Dom
import Control.Monad.Reader

data PlayerColor = PlayerGreen | PlayerBlue | PlayerRed | PlayerCyan | PlayerOrange | PlayerWhite | PlayerBlack deriving (Show, Eq)

allPlayerColors = [PlayerGreen, PlayerBlue, PlayerRed, PlayerCyan, PlayerOrange, PlayerWhite, PlayerBlack]

data SinglePlayerSettings = SinglePlayerSettings {
  playerName :: String,
  playerColor :: PlayerColor,
  settingsPlayerId :: PlayerId
} deriving (Show, Eq)

type PlayerSettings = [SinglePlayerSettings]

initialSettings = []

singlePlayerSettings :: PlayerSettings -> PlayerId -> SinglePlayerSettings
singlePlayerSettings settings playerId =
  let filtered = filter ((== playerId) . settingsPlayerId) settings
      maybeSettings = listToMaybe filtered
  in fromMaybe (SinglePlayerSettings (show playerId) PlayerGreen playerId) maybeSettings

updatePlayerSettings :: SinglePlayerSettings -> PlayerSettings -> PlayerSettings
updatePlayerSettings newSingleSettings oldSettings =
  let filtered = filter ((/= settingsPlayerId newSingleSettings) . settingsPlayerId) oldSettings
  in newSingleSettings : filtered

class ContainsPlayerSettings x t | x -> t where
  extractPlayerSettings :: x -> Dynamic t PlayerSettings

type PlayerSettingsReader t m x = (Reflex t, ContainsPlayerSettings x t, MonadReader x m)

askPlayerSettings :: PlayerSettingsReader t m x => m (Dynamic t PlayerSettings)
askPlayerSettings = asks extractPlayerSettings

askSinglePlayerSettings player = mapDyn (flip singlePlayerSettings player) =<< askPlayerSettings

askPlayerColor player = mapDyn playerColor =<< askSinglePlayerSettings player
