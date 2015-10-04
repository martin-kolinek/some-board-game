module ReflexUtil where

import Reflex
import Reflex.Dom

dynEvent :: MonadWidget t m => Dynamic  t (m (Event t a)) -> m (Event t a)
dynEvent dynamic = do
  inner <- dyn dynamic
  held <- hold never inner
  return $ switch held
