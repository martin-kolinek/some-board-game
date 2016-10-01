{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

import Rules
import Common.DomUtil
import Types
import Board.Dom
import Errors.Dom
import Style

import Reflex.Dom
import Control.Monad.Reader
import Control.Monad.Except

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
  divAttributeLike wrapperClass $ do
    rec
      let applyActionWithTryFinishTurn :: Universe -> UniverseAction -> Either String Universe
          applyActionWithTryFinishTurn universe action = do
            withActionApplied <- action universe
            catchError (finishTurn withActionApplied) (const $ return withActionApplied)
      actionsDyn <- flip runReaderT universeDyn $ do
        actions <- drawBoard
        drawErrors actions
        return actions
      let actionsApplied = attachWith applyActionWithTryFinishTurn (current universeDyn) actionsDyn
          correctMoves = fmapMaybe fromRight actionsApplied
      universeDyn <- holdDyn initialUniverse correctMoves
    return ()
