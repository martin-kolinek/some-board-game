module ReflexUtil where

import Reflex
import Reflex.Dom
import           GHCJS.DOM.Document (documentGetBody)
import           Control.Monad.IO.Class
import CssClass

dynEvent :: MonadWidget t m => (b -> Event t a) -> Dynamic  t (m b) -> m (Event t a)
dynEvent extractEvent dynamic = do
  inner <- dyn dynamic
  held <- hold never (extractEvent <$> inner)
  return $ switch held

dynHold :: Monoid a => MonadWidget t m => Dynamic t (m a) -> m (Dynamic t a)
dynHold dynamic = do
  inner <- dyn dynamic
  holdDyn mempty inner

getBody :: MonadWidget t m => m (El t)
getBody = do
  document <- askDocument
  Just body <- liftIO $ documentGetBody document
  wrapElement body

fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight (Right a) = Just a
fromRight _ = Nothing
