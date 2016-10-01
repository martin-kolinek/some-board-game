{-# LANGUAGE RecursiveDo, TupleSections #-}

module Board.Settings.Dom where

import Rules

import Types
import Board.Settings.Types
import Board.Settings.Style
import Board.Player.Dom
import Board.Worker.Style
import Common.DomUtil

import Reflex.Dom
import Control.Monad
import Data.Map.Strict
import Data.Monoid

drawSettingsIcon :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => m (Dynamic t PlayerSettings)
drawSettingsIcon = do
  rec
    (el, _) <- divAttributeLike' settingsIconClass $ return ()
    let settingsIconClicks = domEvent Click el
        combinedEvents = leftmost [settingsIconClicks, shroudClicks, closePopupClicks]
    settingsVisible <- toggle False combinedEvents
    (settingsChanges, closePopupClicks) <- mapDynExtract drawSettingsWindow settingsVisible
    shroudClicks <- mapDynExtract drawShroud settingsVisible
    postBuild <- getPostBuild
    universe <- askUniverse
    let postBuildSettings = createInitialSettings <$> tag (current universe) postBuild
        settingsEvents = attachWith (flip updatePlayerSettings) (current settingsDyn) settingsChanges
        allSettingsEvents = leftmost [settingsEvents, postBuildSettings]
    settingsDyn <- holdDyn initialSettings allSettingsEvents
  return settingsDyn

createInitialSettings :: Universe -> [SinglePlayerSettings]
createInitialSettings universe =
  let playerNames = (("Player " ++) . show) <$> [(1 :: Int)..]
      zipped = zip (zip playerNames (cycle allPlayerColors)) (getPlayers universe)
  in uncurry (uncurry SinglePlayerSettings) <$> zipped

drawShroud :: MonadWidget t m => Bool -> m (Event t ())
drawShroud False = return never
drawShroud True = do
  (element, _) <- divAttributeLike' shroudClass $ return ()
  return (domEvent Click element)

drawSettingsWindow :: (UniverseReader t m x, PlayerSettingsReader t m x, MonadWidget t m) => Bool -> m (Event t SinglePlayerSettings, Event t ())
drawSettingsWindow False = return (never, never)
drawSettingsWindow True =
  divAttributeLike settingsPopupClass $ do
    closeClicks <- drawSettingsClose
    players <- askPlayers
    playersAsMap <- mapDyn (fromList . fmap (, ())) players
    listOfEvents <- listWithKey playersAsMap drawPlayerSettings
    events <- mapDyn (leftmost . elems) listOfEvents
    return (switch (current events), closeClicks)

drawSettingsClose :: MonadWidget t m => m (Event t ())
drawSettingsClose = do
  (element, _) <- divAttributeLike' settingsPopupClose $ return ()
  return $ domEvent Click element

drawPlayerSettings :: (PlayerSettingsReader t m x, MonadWidget t m) => PlayerId -> Dynamic t () -> m (Event t SinglePlayerSettings)
drawPlayerSettings playerId _ = divAttributeLike settingsLineClass $ do
  currentSettings <- askSinglePlayerSettings playerId
  postBuild <- getPostBuild
  nameInput <- textInput $ def
                        & setValue .~ tag (playerName <$> current currentSettings) postBuild
  colorSelections <- drawColorSelection playerId
  let nameDyn = value nameInput
      createSinglePlayerSettingsWithName oldSettings name = SinglePlayerSettings name (playerColor oldSettings) playerId
      createSinglePlayerSettingsWithColor oldSettings color = SinglePlayerSettings (playerName oldSettings) color playerId
      nameChanges = attachWith createSinglePlayerSettingsWithName (current currentSettings) (updated nameDyn)
      colorChanges = attachWith createSinglePlayerSettingsWithColor (current currentSettings) colorSelections
      filteredChanges = ffilter (uncurry (/=)) $ attach (current currentSettings) (leftmost [nameChanges, colorChanges])
  return $ snd <$> filteredChanges

drawColorSelection :: (PlayerSettingsReader t m x, MonadWidget t m) => PlayerId -> m (Event t PlayerColor)
drawColorSelection playerId = do
  playerSettings <- askSinglePlayerSettings playerId
  currentColor <- mapDyn playerColor playerSettings
  events <- forM allPlayerColors $ drawColor currentColor
  return $ leftmost events

drawColor :: MonadWidget t m => Dynamic t PlayerColor -> PlayerColor -> m (Event t PlayerColor)
drawColor selectedColorDyn color = do
  let cls selectedColor
            | selectedColor == color = colorClass color <> activeWorkerClass
            | otherwise = colorClass color
  classToDraw <- mapDyn cls selectedColorDyn
  (element, _) <- divAttributeLikeDyn' classToDraw $ return ()
  return $ const color <$> domEvent Click element
