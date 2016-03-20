module CssClass(CssClass(),
  divCssClass,
  divCssClassDyn,
  classSelector,
  cardClass,
  cardWrapperClass,
  workerClass,
  idleWorkerContainerClass,
  scoreClass,
  freeWorkersClass,
  errorContainerClass,
  errorItemClass,
  fadeClass,
  appearClass,
  buttonSpanCssClass,
  closeButtonClass,
  activeWorkerClass,
  playerClass,
  selectedPlayerClass,
  playerContainerClass) where

import           Clay as C
import           Data.Text
import           Reflex.Dom

newtype CssClass = CssClass String

instance Monoid CssClass where
  mempty = CssClass ""
  mappend (CssClass a) (CssClass b) = CssClass (a ++ " " ++ b)

extractClassName (CssClass className) = className

divCssClass (CssClass className) = elAttr' "div" ("class" =: className)
divCssClassDyn :: MonadWidget t m => Dynamic t CssClass -> m a -> m (El t, a)
divCssClassDyn cls inner = do
  attrDyn <- mapDyn (("class" =: ) . extractClassName) cls
  elDynAttr' "div" attrDyn inner

classSelector (CssClass className) = byClass $ pack className

buttonSpanCssClass :: MonadWidget t m => CssClass -> m a -> m (Event t ())
buttonSpanCssClass (CssClass className) inside = do
  (el, a) <- elAttr' "span" ("class" =: className) inside
  return $ domEvent Click el

scoreClass = CssClass "score"
freeWorkersClass = CssClass "free-workers"

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
activeWorkerClass = CssClass "active-worker"
fadeClass = CssClass "fade"
appearClass = CssClass "appear"
idleWorkerContainerClass = CssClass "idle-worker-container"
errorContainerClass = CssClass "error-container"
errorItemClass = CssClass "error-item"
closeButtonClass = CssClass "fa fa-times"
playerClass = CssClass "player"
selectedPlayerClass = CssClass "selected-player"
playerContainerClass = CssClass "player-container"
