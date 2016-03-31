{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections #-}

import Rules
import Common.DomUtil
import Types
import Board.Dom
import Errors.Dom
import Style

import Reflex.Dom
import Data.Maybe

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $ do
    rec
      finishActions <- drawFinish universe
      boardActions <- drawBoard universe
      let actions = leftmost [boardActions, finishActions]
      let tryApplyToUniverse action universe = fromMaybe universe $ fromRight $ action universe
      drawErrors universe actions
      universe <- foldDyn tryApplyToUniverse initialUniverse actions
    return ()

drawFinish :: MonadWidget t m => Dynamic t Universe -> m (Event t UniverseAction)
drawFinish universeDyn = do
  (_, event) <- divCssClass finishClass $ button "Finish turn"
  return $ const finishTurn <$> event
