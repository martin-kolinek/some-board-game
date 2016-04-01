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

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
      actions <- flip runReaderT universe $ do
        finishActions <- drawFinish universe
        boardActions <- drawBoard
        let actions = leftmost [boardActions, finishActions]
        drawErrors actions
        return actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()

drawFinish :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawFinish universeDyn = do
  (_, event) <- divCssClass finishClass $ button "Finish turn"
  return $ const finishTurn <$> event
