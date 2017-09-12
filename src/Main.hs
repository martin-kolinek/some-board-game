{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

import Rules
import Common.DomUtil
import Types
import Player.Dom
import Errors.Dom
import Style
import Settings.Dom

import Reflex.Dom.Main as M
import Reflex.Dom
import Control.Monad.Except
import Language.Javascript.JSaddle.Warp as W

main :: IO ()
main = W.run 9000 $ M.mainWidgetWithCss mainStyleByteString $ do
-- main = mainWidgetWithCss mainStyleByteString $ do
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

