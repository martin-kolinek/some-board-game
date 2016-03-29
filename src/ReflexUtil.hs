{-#LANGUAGE TupleSections, ScopedTypeVariables, LambdaCase #-}
module ReflexUtil where

import Reflex
import Reflex.Dom
import qualified GHCJS.DOM.Document as Doc
import GHCJS.DOM.Element
import           Control.Monad.IO.Class
import CssClass
import Data.Time.Clock
import Data.List as L
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function
import Reflex.Host.Class
import Data.Align
import Data.These
import Data.Either

data AnimationState = Initial | Standard | Fading deriving (Show, Eq)

animated :: (MonadWidget t m, Ord k, Show k, Show a) => NominalDiffTime -> Dynamic t (M.Map k a) -> (k -> Dynamic t (AnimationState, a) -> m b) -> m (Dynamic t (M.Map k b))
animated time input draw = do
  postBuild <- getPostBuild
  let diffMapAnim old new = ffor (align old new) $ \case
        This old -> Left old
        That new -> Right new
        These _ new -> Right new
      changeVals = attachWith diffMapAnim (current input) $ leftmost
                     [ updated input
                     , tag (current input) postBuild
                     ]
  let removals = M.mapMaybe fromLeft <$> changeVals
      additions = M.mapMaybe fromRight <$> changeVals
  delayedRemovals <- delay time removals
  let removalsAsNothing = M.map (const Nothing) <$> delayedRemovals
      removalsAsLeft = M.map (Just . Left) <$> removals
      additionsAsRight = M.map (Just . Right) <$> additions
      additionsAndRemovals = mergeWith M.union [additionsAsRight, removalsAsLeft, removalsAsNothing]
  listWithKeyShallowDiff M.empty additionsAndRemovals $ \key initialVal changes -> do
    (event, trigger) <- newEventWithTriggerRef
    innerPostBuild <- getPostBuild
    let leftChanges = fmapMaybe fromLeft changes
        fadings = const Fading <$> leftChanges
        animationStatesEvent = leftmost [event, fadings]
    animationStates <- holdDyn Initial animationStatesEvent
    heldChanges <- holdDyn initialVal changes
    values <- mapDyn (either id id) heldChanges
    combined <- combineDyn (,) (traceDyn "anim" animationStates) values
    result <- draw key combined
    performEvent_ (const (runFrameWithTriggerRef trigger Standard) <$> innerPostBuild)
    performEvent_ (const (runFrameWithTriggerRef trigger Initial) <$> innerPostBuild)
    return result

animatedList :: (MonadWidget t m, Ord k, Show k) => NominalDiffTime -> Dynamic t [k] -> (k -> Dynamic t AnimationState -> m b) -> m (Dynamic t (M.Map k b))
animatedList time input draw = do
  let draw2 key dyn = do
        fstDyn <- mapDyn fst dyn
        draw key fstDyn
  mapFromList <- mapDyn (\x -> M.fromList $ (, ()) <$> x) input
  animated time mapFromList draw2

animatedList2 :: (MonadWidget t m, Ord k, Show k) => NominalDiffTime -> Dynamic t [k] -> (k -> Dynamic t AnimationState -> m b) -> m (Dynamic t (M.Map k b))
animatedList2 time input draw = do
  let draw2 key dyn = draw key (constDyn Standard)
  mapFromList <- mapDyn (\x -> M.fromList $ (, ()) <$> x) input
  listWithKey mapFromList draw2

animateState :: MonadWidget t m => Dynamic t CssClass -> Dynamic t CssClass -> Dynamic t CssClass -> Dynamic t AnimationState -> m a -> m (El t, a)
animateState alwaysOnDyn fadeDyn appearDyn dynamic inner = do
  let combineAll Standard alwaysOn _ appear = alwaysOn <> appear
      combineAll _ alwaysOn fade _ = alwaysOn <> fade
  x <- combineDyn combineAll dynamic alwaysOnDyn
  y <- combineDyn id x fadeDyn
  z <- combineDyn id y appearDyn
  divCssClassDyn z inner

updatedWithInitialValue :: MonadWidget t m => Dynamic t a -> m (Event t a)
updatedWithInitialValue input = do
  postBuild <- getPostBuild
  let tagged = tag (current input) postBuild
      combined = leftmost [updated input, tagged]
  return combined

dynEvent :: MonadWidget t m => (b -> Event t a) -> Dynamic  t (m b) -> m (Event t a)
dynEvent extractEvent dynamic = do
  inner <- dyn dynamic
  held <- hold never (extractEvent <$> inner)
  return $ switch held

dynHold :: Monoid a => MonadWidget t m => Dynamic t (m a) -> m (Dynamic t a)
dynHold dynamic = do
  inner <- dyn dynamic
  holdDyn mempty inner

fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight (Right a) = Just a
fromRight _ = Nothing

filterByBehavior func = attachWithMaybe filter
  where filter a b
          | func a = Just b
          | otherwise = Nothing
