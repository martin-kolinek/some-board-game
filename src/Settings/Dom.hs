{-# LANGUAGE RecursiveDo, TupleSections, OverloadedStrings #-}

module Settings.Dom where

import Rules

import Settings.Types
import Settings.Style
import Player.Worker.Style
import Common.DomUtil
import Common.CommonClasses
import Common.CssClass

import Reflex.Dom
import Control.Monad
import Data.Map.Strict
import Data.Monoid
import qualified Data.Text as T

drawSettingsIcon :: MonadWidget t m => Dynamic t Universe -> m (Dynamic t PlayerSettings)
drawSettingsIcon universeDyn = do
  rec
    (el, _) <- divAttributeLike' settingsIconClass $ return ()
    let settingsIconClicks = domEvent Click el
        combinedEvents = leftmost [settingsIconClicks, closePopupClicks]
    settingsVisible <- toggle False combinedEvents
    (settingsChanges, closePopupClicks) <- drawSettingsWindow universeDyn settingsDyn settingsVisible
    postBuild <- getPostBuild
    let postBuildSettings = createInitialSettings <$> tag (current universeDyn) postBuild
        settingsEvents = attachWith (flip updatePlayerSettings) (current settingsDyn) settingsChanges
        allSettingsEvents = leftmost [settingsEvents, postBuildSettings]
    settingsDyn <- holdDyn initialSettings allSettingsEvents
  return settingsDyn

createInitialSettings :: Universe -> [SinglePlayerSettings]
createInitialSettings universe =
  let playerNames = (T.pack . ("Player " ++) . show) <$> [(1 :: Int)..]
      zipped = zip (zip playerNames (cycle allPlayerColors)) (getPlayers universe)
  in uncurry (uncurry SinglePlayerSettings) <$> zipped

drawShroud :: MonadWidget t m => Dynamic t CssClass -> m (Event t ())
drawShroud classDyn = do
  (shroudElem, _) <- divAttributeLikeDyn' (classDyn <> constDyn shroudClass) $ return ()
  return (domEvent Click shroudElem)

drawSettingsWindow :: MonadWidget t m => Dynamic t Universe -> Dynamic t PlayerSettings -> Dynamic t Bool -> m (Event t SinglePlayerSettings, Event t ())
drawSettingsWindow universeDyn playerSettingsDyn visibleDyn = do
  let hideClassDyn = whenClass (not <$> visibleDyn) hideClass
  shroudClicks <- drawShroud hideClassDyn
  divAttributeLikeDyn (constDyn settingsPopupClass <> hideClassDyn) $ do
    closeClicks <- drawSettingsClose
    let players = getPlayers <$> universeDyn
        playersAsMap = (fromList . fmap (, ())) <$> players
    listOfEvents <- listWithKey playersAsMap (drawPlayerSettings playerSettingsDyn)
    return (switch (leftmost . elems <$> current listOfEvents), closeClicks <> shroudClicks)

drawSettingsClose :: MonadWidget t m => m (Event t ())
drawSettingsClose = do
  (closeElem, _) <- divAttributeLike' settingsPopupClose $ return ()
  return $ domEvent Click closeElem

drawPlayerSettings :: MonadWidget t m => Dynamic t PlayerSettings -> PlayerId -> Dynamic t () -> m (Event t SinglePlayerSettings)
drawPlayerSettings playerSettingsDyn playerId _ = divAttributeLike settingsLineClass $ do
  let currentSettings = (flip singlePlayerSettings playerId) <$> playerSettingsDyn
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
  let currentColor = playerColor <$> playerSettings
  events <- forM allPlayerColors $ drawColor currentColor
  return $ leftmost events

drawColor :: MonadWidget t m => Dynamic t PlayerColor -> PlayerColor -> m (Event t PlayerColor)
drawColor selectedColorDyn color = do
  let cls selectedColor
            | selectedColor == color = colorClass color <> activeWorkerClass
            | otherwise = colorClass color
      classToDraw = cls <$> selectedColorDyn
  (colorElem, _) <- divAttributeLikeDyn' classToDraw $ return ()
  return $ const color <$> domEvent Click colorElem
