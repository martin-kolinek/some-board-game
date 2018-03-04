{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, LambdaCase,
   MultiParamTypeClasses, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
module Common.DomUtil where

import Reflex
import Reflex.Dom
import Common.CssClass
import Data.Time.Clock
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Align
import Data.These
import Clay (Css, renderWith, compact)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data AnimationState = Initial | Standard | Fading deriving (Show, Eq)

animated :: (MonadWidget t m, Ord k) => NominalDiffTime -> Dynamic t (M.Map k a) -> (k -> Dynamic t (AnimationState, a) -> m b) -> m (Dynamic t (M.Map k b))
animated time input draw = do
  postBuild <- getPostBuild
  delayed <- delay time $ leftmost [updated input, tag (current input) postBuild]
  delayedInput <- holdDyn M.empty delayed
  let aligned = align <$> input <*> delayedInput
      extractTuple (This a) = (Standard, a)
      extractTuple (That a) = (Fading, a)
      extractTuple (These a _) = (Standard, a)
      tupleMapDyn = M.map extractTuple <$> aligned
  listWithKey tupleMapDyn draw

animatedList :: (MonadWidget t m, Ord k) => NominalDiffTime -> Dynamic t [k] -> (k -> Dynamic t AnimationState -> m b) -> m (Dynamic t (M.Map k b))
animatedList time input draw = do
  let draw2 key dyn = draw key (fst <$> dyn)
      mapFromList = (\x -> M.fromList $ (, ()) <$> x) <$> input
  animated time mapFromList draw2

animateState :: MonadWidget t m => Dynamic t CssClass -> Dynamic t CssClass -> Dynamic t AnimationState -> m a -> m (El t, a)
animateState alwaysOnDyn fadeDyn dynamic inner = do
  let combineAll Standard alwaysOn _ = alwaysOn
      combineAll _ alwaysOn fade = alwaysOn <> fade
      y = combineAll <$> dynamic <*> alwaysOnDyn <*> fadeDyn
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
divAttributeLikeDyn' cls inner =
  elDynAttr' "div" (toAttributeMap <$> cls) inner

divAttributeLike :: (MonadWidget t f, AttributeLike t1) => t1 -> f b -> f b
divAttributeLike atr a = snd <$> divAttributeLike' atr a

divAttributeLikeDyn :: (MonadWidget t f, AttributeLike atr) => Dynamic t atr -> f b -> f b
divAttributeLikeDyn atr a = snd <$> divAttributeLikeDyn' atr a

classAttribute :: CssClass -> M.Map T.Text T.Text
classAttribute (CssClass className) = M.singleton "class" className

buttonSpanCssClass :: MonadWidget t m => CssClass -> m a -> m (Event t ())
buttonSpanCssClass (CssClass className) inside = do
  (el, _) <- elAttr' "span" ("class" =: className) inside
  return $ domEvent Click el

leftmostPair :: Reflex t => [(Event t a, Event t b)] -> (Event t a, Event t b)
leftmostPair events = (leftmost (fst <$> events), leftmost (snd <$> events))

styleStringFromCss :: Css -> T.Text
styleStringFromCss = T.tail . T.init . LT.toStrict . renderWith compact []

class AttributeLike t where
  toAttributeMap :: t -> M.Map T.Text T.Text

instance AttributeLike CssClass where
  toAttributeMap (CssClass cls) = "class" =: cls

instance AttributeLike Css where
  toAttributeMap css = (T.pack "style") =: styleStringFromCss css

instance AttributeLike (M.Map T.Text T.Text) where
  toAttributeMap = id

instance (AttributeLike a, AttributeLike b) => AttributeLike (a, b) where
  toAttributeMap (a, b) = toAttributeMap a <> toAttributeMap b

instance AttributeLike () where
  toAttributeMap _ = M.empty

whenClass :: (Monoid b, Functor f) => f Bool -> b -> f b
whenClass cond cls = classDyn <$> cond
  where classDyn True = cls
        classDyn False = mempty

simpleListOrd :: (MonadWidget t m, Ord a) => Dynamic t [a] -> (a -> m  b) -> m (Dynamic t [b])
simpleListOrd listDyn itemFunc =
  let toMap = M.fromList . fmap (, 1 :: Int)
      applyFunc k _ = itemFunc k
  in fmap M.elems <$> listWithKey (toMap <$> listDyn) applyFunc

whenWidget :: MonadWidget t m => Dynamic t Bool -> m () -> m ()
whenWidget cond inner = do
  heldCond <- holdUniqDyn cond
  postBuild <- getPostBuild
  let draw b = if b then inner else return ()
      updates = draw <$> updated heldCond
      initial = draw <$> tag (current heldCond) postBuild
  _ <- widgetHold (return ()) $ leftmost [initial, updates]
  return ()
