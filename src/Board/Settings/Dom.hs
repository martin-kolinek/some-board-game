{-# LANGUAGE RecursiveDo #-}

module Board.Settings.Dom where

import Board.Settings.Types
import Board.Settings.Style
import Common.DomUtil

import Reflex.Dom

drawSettingsIcon :: MonadWidget t m => m (Dynamic t PlayerSettings)
drawSettingsIcon = do
  rec
    (el, _) <- divCssClass settingsIconClass $ return ()
    let settingsIconClicks = domEvent Click el
        combinedEvents = leftmost [settingsIconClicks, shroudClicks]
    settingsVisible <- toggle False $ combinedEvents
    settingsChanges <- mapDynExtract drawSettingsWindow settingsVisible
    shroudClicks <- mapDynExtract drawShroud settingsVisible
  foldDyn updatePlayerSettings initialSettings settingsChanges

drawShroud :: MonadWidget t m => Bool -> m (Event t ())
drawShroud False = return never
drawShroud True = do
  (el, _) <- divCssClass shroudClass $ return ()
  return (domEvent Click el)

drawSettingsWindow :: MonadWidget t m => Bool -> m (Event t SinglePlayerSettings)
drawSettingsWindow False = return never
drawSettingsWindow True = do
  (_, result) <- divCssClass settingsPopupClass $ do
    return never
  return result
