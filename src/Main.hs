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
import Language.Javascript.JSaddle.WebSockets as W
import Network.Wai.Handler.Warp as Net (run)
import Network.WebSockets (defaultConnectionOptions)
import Language.Javascript.JSaddle.Run (syncPoint)
import Network.Wai.Middleware.Static (static)
import Language.Javascript.JSaddle.Types (JSM)

mainJsm :: JSM ()
mainJsm = M.mainWidgetWithCss mainStyleByteString $ do
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

main :: IO ()
main = do
  app <- W.jsaddleOr defaultConnectionOptions (mainJsm >> syncPoint) W.jsaddleApp
  Net.run 9000 $ static app
