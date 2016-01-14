module CssClass(CssClass(), divCssClass, divCssClassDyn, classSelector, cardClass, cardWrapperClass, workerClass, idleWorkerContainerClass, scoreClass, freeWorkersClass, errorContainerClass, errorItemClass, fadeClass) where

import           Clay as C
import           Data.Text
import           Reflex.Dom

newtype CssClass = CssClass String

extractClassName (CssClass className) = className

divCssClass (CssClass className) = elAttr' "div" ("class" =: className)
divCssClassDyn :: MonadWidget t m => Dynamic t [CssClass] -> m a -> m (El t, a)
divCssClassDyn classes inner = do
  let joinClasses classes = C.intersperse " " (extractClassName <$> classes)
  attrDyn <- mapDyn (("class" =: ) . joinClasses) classes
  elDynAttr' "div" attrDyn inner

classSelector (CssClass className) = byClass $ pack className

scoreClass = CssClass "score"
freeWorkersClass = CssClass "free-workers"

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
idleWorkerContainerClass = CssClass "idle-worker-container"
errorContainerClass = CssClass "error-container"
errorItemClass = CssClass "error-item"
fadeClass = CssClass "fade"
