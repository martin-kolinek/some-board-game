{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, LambdaCase,
   MultiParamTypeClasses, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
module Common.DomUtil where

import Reflex
import Reflex.Dom
import Common.CssClass
import Data.Monoid
import qualified Data.Map.Strict as M
import Clay (Css, renderWith, compact)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

simplerList :: (MonadWidget t m, Ord k) => Dynamic t [k] -> (k -> m a) -> m (Dynamic t (M.Map k a))
simplerList input draw = do
  let draw2 key _ = draw key
      mapFromList = (\x -> M.fromList $ (, ()) <$> x) <$> input
  listWithKey mapFromList draw2

data KeyValWrapper k v = KeyValWrapper k v
instance Eq k => Eq (KeyValWrapper k v)
  where (KeyValWrapper k _) == (KeyValWrapper k2 _) = k == k2
instance Ord k => Ord (KeyValWrapper k v)
  where compare (KeyValWrapper k1 _) (KeyValWrapper k2 _) = compare k1 k2

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
  let toMap False = M.empty
      toMap True = M.singleton () ()
  _ <- listWithKey (toMap <$> cond) $ \_ _ -> inner
  return ()
