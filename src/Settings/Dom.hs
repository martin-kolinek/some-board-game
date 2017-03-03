{-# LANGUAGE RecursiveDo, TupleSections #-}

module Settings.Dom where

import Rules

import Settings.Types
import Settings.Style
import Player.Worker.Style
import Common.DomUtil

import Reflex.Dom
import Control.Monad
import Data.Map.Strict
import Data.Monoid

drawSettingsIcon :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t PlayerSettings)
drawSettingsIcon universeDyn = do
  rec
    (el, _) <- divAttributeLike' settingsIconClass $ return ()
    let settingsIconClicks = domEvent Click el
        combinedEvents = leftmost [settingsIconClicks, shroudClicks, closePopupClicks]
    settingsVisible <- toggle False combinedEvents
    (settingsChanges, closePopupClicks) <- mapDynExtract (drawSettingsWindow universeDyn settingsDyn) settingsVisible
    shroudClicks <- mapDynExtract drawShroud settingsVisible
    postBuild <- getPostBuild
    let postBuildSettings = createInitialSettings <$> tag (current universeDyn) postBuild
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

drawSettingsWindow :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> Bool -> m (Event t SinglePlayerSettings, Event t ())
drawSettingsWindow _ _ False = return (never, never)
drawSettingsWindow universeDyn playerSettingsDyn True =
  divAttributeLike settingsPopupClass $ do
    closeClicks <- drawSettingsClose
    players <- mapDyn getPlayers universeDyn
    playersAsMap <- mapDyn (fromList . fmap (, ())) players
    listOfEvents <- listWithKey playersAsMap (drawPlayerSettings playerSettingsDyn)
    events <- mapDyn (leftmost . elems) listOfEvents
    return (switch (current events), closeClicks)

drawSettingsClose :: MonadWidget t m => m (Event t ())
drawSettingsClose = do
  (element, _) <- divAttributeLike' settingsPopupClose $ return ()
  return $ domEvent Click element

drawPlayerSettings :: MonadWidget t m => Dynamic t PlayerSettings -> PlayerId -> Dynamic t () -> m (Event t SinglePlayerSettings)
drawPlayerSettings playerSettingsDyn playerId _ = divAttributeLike settingsLineClass $ do
  currentSettings <- mapDyn (flip singlePlayerSettings playerId) playerSettingsDyn
  postBuild <- getPostBuild
  nameInput <- textInput $ def
                        & setValue .~ tag (playerName <$> current currentSettings) postBuild
  colorSelections <- drawColorSelection currentSettings
  let nameDyn = value nameInput
      createSinglePlayerSettingsWithName oldSettings name = SinglePlayerSettings name (playerColor oldSettings) playerId
      createSinglePlayerSettingsWithColor oldSettings color = SinglePlayerSettings (playerName oldSettings) color playerId
      nameChanges = attachWith createSinglePlayerSettingsWithName (current currentSettings) (updated nameDyn)
      colorChanges = attachWith createSinglePlayerSettingsWithColor (current currentSettings) colorSelections
      filteredChanges = ffilter (uncurry (/=)) $ attach (current currentSettings) (leftmost [nameChanges, colorChanges])
  return $ snd <$> filteredChanges

drawColorSelection :: MonadWidget t m => Dynamic t SinglePlayerSettings -> m (Event t PlayerColor)
drawColorSelection playerSettings = do
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
