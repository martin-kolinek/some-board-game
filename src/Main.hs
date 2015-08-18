{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

import           Control.Monad
import           CssClass
import           Reflex
import           Reflex.Dom
import           Style

main :: IO ()
main = mainWidgetWithCss mainStyleByteString $
  forM_ [1..15] $ const card

card :: MonadWidget t m => m ()
card = do
  rec
    (wrapper, _) <- divCssClass cardWrapperClass $
                      divCssClass cardClass $
                        dyn workerDiv
    let cardClicks = domEvent Click wrapper
    working <- foldDyn (const not) False cardClicks
    workerDiv <- worker `mapDyn` working
  return ()

worker :: MonadWidget t m => Bool -> m Bool
worker True = do
  _ <- divCssClass workerClass $ return ()
  return True
worker False = return False
