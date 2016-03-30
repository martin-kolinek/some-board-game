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
  buttonSpanCssClass,
  closeButtonClass,
  activeWorkerClass,
  playerClass,
  selectedPlayerClass,
  currentPlayerClass,
  playerContainerClass) where

import           Clay as C
import           Data.Text
import           Reflex.Dom
import           Data.String

newtype CssClass = CssClass String deriving Show

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

animatedClass (CssClass className) duration fadeCss = do
  let animName = "anim-" ++ className
  keyframesFromTo (pack animName) fadeCss (return ())
  C.star # C.byClass (pack className) # classSelector fadeClass ? fadeCss
  C.star # C.byClass (pack className) ? do
    animationName (fromString animName)
    animationDuration duration
    animationIterationCount $ iterationCount 1

scoreClass = CssClass "score"
freeWorkersClass = CssClass "free-workers"

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
activeWorkerClass = CssClass "active-worker"
fadeClass = CssClass "fade"
idleWorkerContainerClass = CssClass "idle-worker-container"
errorContainerClass = CssClass "error-container"
errorItemClass = CssClass "error-item"
closeButtonClass = CssClass "fa fa-times"
playerClass = CssClass "player"
selectedPlayerClass = CssClass "selected-player"
currentPlayerClass = CssClass "current-player"
playerContainerClass = CssClass "player-container"
