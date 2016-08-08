{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

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

instance ContainsUniverse (Dynamic t Universe) t where extractUniverse = id

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
  divAttributeLike wrapperClass $ do
    rec
      let applyActionWithTryFinishTurn :: Universe -> UniverseAction -> Either String Universe
          applyActionWithTryFinishTurn universe action = do
            withActionApplied <- action universe
            catchError (finishTurn withActionApplied) (const $ return withActionApplied)
      actions <- flip runReaderT universe $ do
        actions <- drawBoard
        drawErrors actions
        return actions
      let actionsApplied = attachWith applyActionWithTryFinishTurn (current universe) actions
          correctMoves = fmapMaybe fromRight actionsApplied
      universe <- holdDyn initialUniverse correctMoves
    return ()
