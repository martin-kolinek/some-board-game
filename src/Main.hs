{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections #-}

import Rules
import Common.DomUtil
import Types
import Board.Dom
import Errors.Dom
import Style

import Reflex.Dom
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      let applyActionWithTryFinishTurn :: UniverseAction -> Universe -> Either String Universe
          applyActionWithTryFinishTurn action universe = do
            withActionApplied <- action universe
            catchError (finishTurn withActionApplied) (const $ return withActionApplied)
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ applyActionWithTryFinishTurn action universe
      actions <- flip runReaderT universe $ do
        actions <- drawBoard
        drawErrors actions
        return actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()
