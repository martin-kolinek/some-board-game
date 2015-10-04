module ReflexUtil where

import Reflex
import Reflex.Dom
import           GHCJS.DOM.Document (documentGetBody)
import           Control.Monad.IO.Class

dynEvent :: MonadWidget t m => Dynamic  t (m (Event t a)) -> m (Event t a)
dynEvent dynamic = do
  inner <- dyn dynamic
  held <- hold never inner
  return $ switch held

getBody :: MonadWidget t m => m (El t)
getBody = do
  document <- askDocument
  Just body <- liftIO $ documentGetBody document
  wrapElement body

fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight (Right a) = Just a
fromRight _ = Nothing
