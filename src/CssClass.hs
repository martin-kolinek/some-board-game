module CssClass(divCssClass, classSelector, cardClass, cardWrapperClass, workerClass, idleWorkerContainerClass, scoreClass) where

import           Clay
import           Data.Text
import           Reflex.Dom

newtype CssClass = CssClass String

divCssClass (CssClass className) = elAttr' "div" ("class" =: className)
classSelector (CssClass className) = byClass $ pack className

scoreClass = CssClass "score"

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
idleWorkerContainerClass = CssClass "idle-worker-container"
