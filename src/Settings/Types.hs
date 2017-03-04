{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}

module Settings.Types where

import Rules

import Data.Maybe
import qualified Data.Text as T

data PlayerColor = PlayerGreen | PlayerBlue | PlayerRed | PlayerCyan | PlayerOrange | PlayerWhite | PlayerBlack deriving (Show, Eq)

allPlayerColors :: [PlayerColor]
allPlayerColors = [PlayerGreen, PlayerBlue, PlayerRed, PlayerCyan, PlayerOrange, PlayerWhite, PlayerBlack]

data SinglePlayerSettings = SinglePlayerSettings {
  playerName :: T.Text,
  playerColor :: PlayerColor,
  settingsPlayerId :: PlayerId
} deriving (Show, Eq)

type PlayerSettings = [SinglePlayerSettings]

initialSettings :: PlayerSettings
initialSettings = []

singlePlayerSettings :: PlayerSettings -> PlayerId -> SinglePlayerSettings
singlePlayerSettings settings playerId =
  let filtered = filter ((== playerId) . settingsPlayerId) settings
      maybeSettings = listToMaybe filtered
  in fromMaybe (SinglePlayerSettings (T.pack $ show playerId) PlayerGreen playerId) maybeSettings

updatePlayerSettings :: SinglePlayerSettings -> PlayerSettings -> PlayerSettings
updatePlayerSettings newSingleSettings oldSettings =
  let filtered = filter ((/= settingsPlayerId newSingleSettings) . settingsPlayerId) oldSettings
  in newSingleSettings : filtered
