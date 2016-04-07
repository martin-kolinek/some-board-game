{-# LANGUAGE RecursiveDo, TupleSections #-}

module Board.Settings.Dom where

import Rules

import Types
import Board.Settings.Types
import Board.Settings.Style
import Board.Player.Dom
import Common.DomUtil
import Data.Map.Strict

import Reflex.Dom
import Control.Monad

drawSettingsIcon :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (Dynamic t PlayerSettings)
drawSettingsIcon = do
  rec
    (el, _) <- divCssClass settingsIconClass $ return ()
    let settingsIconClicks = domEvent Click el
        combinedEvents = leftmost [settingsIconClicks, shroudClicks, closePopupClicks]
    settingsVisible <- toggle False combinedEvents
    (settingsChanges, closePopupClicks) <- mapDynExtract drawSettingsWindow settingsVisible
    shroudClicks <- mapDynExtract drawShroud settingsVisible
  foldDyn updatePlayerSettings initialSettings settingsChanges

drawShroud :: MonadWidget t m => Bool -> m (Event t ())
drawShroud False = return never
drawShroud True = do
  (el, _) <- divCssClass shroudClass $ return ()
  return (domEvent Click el)

drawSettingsWindow :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Bool -> m (Event t SinglePlayerSettings, Event t ())
drawSettingsWindow False = return (never, never)
drawSettingsWindow True = do
  (_, result) <- divCssClass settingsPopupClass $ do
    closeClicks <- drawSettingsClose
    players <- askPlayers
    playersAsMap <- mapDyn (fromList . fmap (, ())) players
    listOfEvents <- listWithKey playersAsMap drawPlayerSettings
    events <- mapDyn (leftmost . elems) listOfEvents
    return (switch (current events), closeClicks)
  return result

drawSettingsClose :: MonadWidget t m => m (Event t ())
drawSettingsClose = do
  (el, _) <- divCssClass settingsPopupClose $ return ()
  return $ domEvent Click el

drawPlayerSettings :: (PlayerSettingsReader t m x, MonadWidget t m) => PlayerId -> Dynamic t () -> m (Event t SinglePlayerSettings)
drawPlayerSettings playerId _ = do
  currentSettings <- askSinglePlayerSettings playerId
  postBuild <- getPostBuild
  input <- textInput $ def
                        & setValue .~ tag (playerName <$> current currentSettings) postBuild
  let nameDyn = value input
      createSinglePlayerSettings name = SinglePlayerSettings name PlayerGreen playerId
      filteredChanges = traceEvent "aaa" $ ffilter (uncurry (/=)) $ attach (playerName <$> current currentSettings) (updated nameDyn)
  return $ createSinglePlayerSettings <$> (snd <$> filteredChanges)
