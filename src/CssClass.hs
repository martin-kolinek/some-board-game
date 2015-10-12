{-# LANGUAGE RecursiveDo #-}

module CssClass(divCssClass, classSelector, cardClass, cardWrapperClass, workerClass, idleWorkerContainerClass, scoreClass, freeWorkersClass, errorContainerClass, errorItemClass, divAnimFromWithClass, testClass) where

import           Clay
import           Control.Monad.IO.Class
import           Data.Text
import           GHCJS.DOM.Element
import           Reflex.Dom
import           Reflex.Dom.Widget.Basic
import           Data.Time
import           Reflex.Host.Class
import           Data.Monoid
import           Data.Map
import           Debug.Trace

newtype CssClass = CssClass String

divCssClass (CssClass className) = elAttr' "div" ("class" =: className)
classSelector (CssClass className) = byClass $ pack className

divAnimFromWithClass :: MonadWidget t m => CssClass -> El t -> m a -> m (El t, a)
divAnimFromWithClass (CssClass className) origin inner = do
  rec
    let htmlEl = _el_element origin
    originTop <- liftIO $ elementGetClientTop htmlEl
    originLeft <- liftIO $ elementGetClientLeft htmlEl
    (ev, trigger) <- newEventWithTriggerRef
    result@(el, _) <- elDynAttr' "div" (traceDyn "aaa" attrs) inner
    let newHtmlEl = _el_element el
    newTop <- liftIO $ elementGetClientTop newHtmlEl
    newLeft <- liftIO $ elementGetClientLeft newHtmlEl
    let left = originLeft - (traceShowId newLeft)
        top = originTop - (traceShowId newTop)
        initial = "class" =: className <> "style" =: "display:none;"
        middle = "class" =: className <> "style" =: ("left: " ++ show left ++ "px; top: " ++ show top ++ "px; position: relative;")
        end = "class" =: className
        evF :: Int -> Map String String -> Map String String
        evF 1 _ = middle
        evF _ _ = end
    attrs <- foldDyn evF initial ev
    runFrameWithTriggerRef trigger 1
    --runFrameWithTriggerRef trigger 2
  return result

scoreClass = CssClass "score"
freeWorkersClass = CssClass "free-workers"

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workerClass = CssClass "worker"
idleWorkerContainerClass = CssClass "idle-worker-container"
errorContainerClass = CssClass "error-container"
errorItemClass = CssClass "error-item"
testClass = CssClass "test"
