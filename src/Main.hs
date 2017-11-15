{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, CPP #-}

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
import Language.Javascript.JSaddle.Types (JSM)
#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp as Net (run)
import Network.WebSockets (defaultConnectionOptions)
import Language.Javascript.JSaddle.Run (syncPoint)
import Network.Wai.Middleware.Static (static)
import Language.Javascript.JSaddle.Warp as W
#endif

mainJsm :: JSM ()
mainJsm = M.mainWidgetWithCss mainStyleByteString $ do
    rec
      let applyActionWithTryFinishTurn :: Universe -> UniverseAction -> Either String Universe
          applyActionWithTryFinishTurn universe action = do
            withActionApplied <- applyAction action universe
            catchError (finishTurn withActionApplied) (const $ return withActionApplied)
      settingsDyn <- drawSettingsIcon universeDyn
      actions <- drawPlayers universeDyn settingsDyn
      drawErrors universeDyn actions
      let actionsApplied = attachWith applyActionWithTryFinishTurn (current universeDyn) actions
          correctMoves = fmapMaybe fromRight actionsApplied
      universeDyn <- holdDyn initialUniverse correctMoves
    return ()

#ifdef ghcjs_HOST_OS
main :: IO ()
main = mainJsm
#else
main :: IO ()
main = do
  app <- W.jsaddleOr defaultConnectionOptions (mainJsm >> syncPoint) W.jsaddleApp
  Net.run 9000 $ static app
#endif

