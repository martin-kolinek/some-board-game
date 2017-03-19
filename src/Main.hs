{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

import Rules
import Common.DomUtil
import Types
import Player.Dom
import Errors.Dom
import Style
import Settings.Dom

import Reflex.Dom
import Control.Monad.Except

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
  rec
    let applyActionWithTryFinishTurn :: Universe -> UniverseAction -> Either String Universe
        applyActionWithTryFinishTurn universe action = do
          withActionApplied <- action universe
          catchError (finishTurn withActionApplied) (const $ return withActionApplied)
    settingsDyn <- drawSettingsIcon universeDyn
    actions <- drawPlayers universeDyn settingsDyn
    drawErrors universeDyn actions
    let actionsApplied = attachWith applyActionWithTryFinishTurn (current universeDyn) actions
        correctMoves = fmapMaybe fromRight actionsApplied
    universeDyn <- holdDyn initialUniverse correctMoves
  return ()

