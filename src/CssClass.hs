module CssClass(divCssClass, classSelector, cardClass, cardWrapperClass, workerClass) where

import           Clay
import           Data.Text
import           Reflex.Dom

newtype CssClass = CssClass String

divCssClass (CssClass className) = elAttr' "div" ("class" =: className)
classSelector (CssClass className) = byClass $ pack className

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
