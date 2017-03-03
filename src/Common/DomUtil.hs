{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, LambdaCase,
   MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}
module Common.DomUtil where

import Reflex
import Reflex.Dom
import Common.CssClass
import Data.Time.Clock
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Align
import Data.These
import Data.Default
import Clay (Css, renderWith, compact)
import Data.Text.Lazy as T (unpack, tail, init)

data AnimationState = Initial | Standard | Fading deriving (Show, Eq)

animated :: (MonadWidget t m, Ord k, Show k, Show a) => NominalDiffTime -> Dynamic t (M.Map k a) -> (k -> Dynamic t (AnimationState, a) -> m b) -> m (Dynamic t (M.Map k b))
animated time input draw = do
  postBuild <- getPostBuild
  delayed <- delay time $ leftmost [updated input, tag (current input) postBuild]
  delayedInput <- holdDyn M.empty delayed
  aligned <- combineDyn align input delayedInput
  let extractTuple (This a) = (Standard, a)
      extractTuple (That a) = (Fading, a)
      extractTuple (These a _) = (Standard, a)
  tupleMapDyn <- mapDyn (M.map extractTuple) aligned
  listWithKey tupleMapDyn draw

animatedList :: (MonadWidget t m, Ord k, Show k) => NominalDiffTime -> Dynamic t [k] -> (k -> Dynamic t AnimationState -> m b) -> m (Dynamic t (M.Map k b))
animatedList time input draw = do
  let draw2 key dyn = do
        fstDyn <- mapDyn fst dyn
        draw key fstDyn
  mapFromList <- mapDyn (\x -> M.fromList $ (, ()) <$> x) input
  animated time mapFromList draw2

animateState :: MonadWidget t m => Dynamic t CssClass -> Dynamic t CssClass -> Dynamic t AnimationState -> m a -> m (El t, a)
animateState alwaysOnDyn fadeDyn dynamic inner = do
  let combineAll Standard alwaysOn _ = alwaysOn
      combineAll _ alwaysOn fade = alwaysOn <> fade
  x <- combineDyn combineAll dynamic alwaysOnDyn
  y <- combineDyn id x fadeDyn
  divAttributeLikeDyn' y inner

updatedWithInitialValue :: MonadWidget t m => Dynamic t a -> m (Event t a)
updatedWithInitialValue input = do
  postBuild <- getPostBuild
  let tagged = tag (current input) postBuild
      combined = leftmost [updated input, tagged]
  return combined

fromLeft :: forall t a. Either a t -> Maybe a
fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight :: forall t a. Either t a -> Maybe a
fromRight (Right a) = Just a
fromRight _ = Nothing

filterByBehavior :: forall t c t1. Reflex t1 => (t -> Bool) -> Behavior t1 t -> Event t1 c -> Event t1 c
filterByBehavior func = attachWithMaybe filter
  where filter a b
          | func a = Just b
          | otherwise = Nothing

divAttributeLike' :: (MonadWidget t m, AttributeLike t1) => t1 -> m a -> m (El t, a)
divAttributeLike' atr = elAttr' "div" (toAttributeMap atr)

divAttributeLikeDyn' :: (AttributeLike atr, MonadWidget t m) => Dynamic t atr -> m a -> m (El t, a)
divAttributeLikeDyn' cls inner = do
  attrDyn <- mapDyn toAttributeMap cls
  elDynAttr' "div" attrDyn inner

divAttributeLike :: (MonadWidget t f, AttributeLike t1) => t1 -> f b -> f b
divAttributeLike atr a = snd <$> divAttributeLike' atr a

classAttribute :: CssClass -> M.Map [Char] String
classAttribute (CssClass className) = M.singleton "class" className

buttonSpanCssClass :: MonadWidget t m => CssClass -> m a -> m (Event t ())
buttonSpanCssClass (CssClass className) inside = do
  (el, _) <- elAttr' "span" ("class" =: className) inside
  return $ domEvent Click el

class MonadWidget t m => ExtractableFromEvent t m b where
   extractFromEvent :: Event t b -> m b

instance MonadWidget t m => ExtractableFromEvent t m (Event t b) where
  extractFromEvent event = do
    held <- hold never event
    return $ switch held

instance (MonadWidget t m, Default b) => ExtractableFromEvent t m (Dynamic t b) where
  extractFromEvent event = do
    held <- holdDyn (constDyn def) event
    return $ joinDyn held

instance (MonadWidget t m, ExtractableFromEvent t m a, ExtractableFromEvent t m b) => ExtractableFromEvent t m (a, b) where
  extractFromEvent event = do
    let aev = fst <$> event
    let bev = snd <$> event
    a <- extractFromEvent aev
    b <- extractFromEvent bev
    return (a, b)

instance MonadWidget t m => ExtractableFromEvent t m () where
  extractFromEvent _ = return ()

mapDynExtract :: (ExtractableFromEvent t m b, MonadWidget t m) => (a -> m b) -> Dynamic t a -> m b
mapDynExtract func dynamic = do
  mapped <- mapDyn func dynamic
  dyned <- dyn mapped
  extractFromEvent dyned

forDynExtract :: (ExtractableFromEvent t m b, MonadWidget t m) => Dynamic t a -> (a -> m b) -> m b
forDynExtract = flip mapDynExtract

leftmostPair :: Reflex t => [(Event t a, Event t b)] -> (Event t a, Event t b)
leftmostPair events = (leftmost (fst <$> events), leftmost (snd <$> events))

styleStringFromCss :: Css -> String
styleStringFromCss = unpack . T.tail . T.init . renderWith compact []

combineDyn3 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> m (Dynamic t d)
combineDyn3 f a b c = do
  x <- combineDyn f a b
  combineDyn id x c

class AttributeLike t where
  toAttributeMap :: t -> M.Map String String

instance AttributeLike CssClass where
  toAttributeMap (CssClass cls) = "class" =: cls

instance AttributeLike Css where
  toAttributeMap css = "style" =: styleStringFromCss css

instance AttributeLike (M.Map String String) where
  toAttributeMap = id

instance (AttributeLike a, AttributeLike b) => AttributeLike (a, b) where
  toAttributeMap (a, b) = toAttributeMap a <> toAttributeMap b

instance AttributeLike () where
  toAttributeMap _ = M.empty
