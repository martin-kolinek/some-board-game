{-#LANGUAGE TupleSections #-}
module ReflexUtil where

import Reflex
import Reflex.Dom
import           GHCJS.DOM.Document (documentGetBody)
import           Control.Monad.IO.Class
import CssClass
import Data.Time.Clock
import Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function

data AnimationState = Initial | Standard | Fading deriving (Show, Eq)

animateMap :: MonadWidget t m => Show a => Ord k => NominalDiffTime -> Dynamic t (M.Map k a) -> m (Dynamic t (M.Map k (a, AnimationState)))
animateMap time input = do
  let findInitials nonDelayedList delayed1List = (,Initial) <$> (nonDelayedList M.\\ delayed1List)
      findFading delayed1List delayedTimeList = (,Fading) <$> (delayedTimeList M.\\ delayed1List)
      findStandard nonDelayedList delayed1List delayedTimeList = (,Standard) <$> (delayed1List `M.intersection` (nonDelayedList `M.union` delayedTimeList))
  updatedInput <- updatedWithInitialValue input
  delayed1 <- delay 0.0001 updatedInput
  delayed1Dyn <- holdDyn M.empty delayed1
  delayedTime <- delay time updatedInput
  delayedTimeDyn <- holdDyn M.empty delayedTime
  initials <- combineDyn findInitials input delayed1Dyn
  combined1 <- combineDyn (,) input delayed1Dyn
  combined2 <- combineDyn (,) combined1 delayedTimeDyn
  standard <- mapDyn (uncurry (uncurry findStandard)) combined2
  fading <- combineDyn findFading delayed1Dyn delayedTimeDyn
  mconcatDyn [initials, standard, fading]

animateList :: MonadWidget t m => Show a => Ord a => NominalDiffTime -> Dynamic t [a] -> m (Dynamic t (M.Map a AnimationState))
animateList time input = do
    let findInitials nonDelayedList delayed1List = (Initial,) <$> (nonDelayedList \\ delayed1List)
        findFading delayed1List delayedTimeList = (Fading,) <$> (delayedTimeList \\ delayed1List)
        findStandard nonDelayedList delayed1List delayedTimeList = (Standard,) <$> (delayed1List `intersect` (nonDelayedList `union` delayedTimeList))
    updatedInput <- updatedWithInitialValue input
    delayed1 <- delay 0.0001 updatedInput
    delayed1Dyn <- holdDyn [] delayed1
    delayedTime <- delay time updatedInput
    delayedTimeDyn <- holdDyn [] delayedTime
    initials <- combineDyn findInitials input delayed1Dyn
    combined1 <- combineDyn (,) input delayed1Dyn
    combined2 <- combineDyn (,) combined1 delayedTimeDyn
    standard <- mapDyn (uncurry (uncurry findStandard)) combined2
    fading <- combineDyn findFading delayed1Dyn delayedTimeDyn
    allStates <- mconcatDyn [initials, standard, fading]
    let combineAnimationStates states = head $
          catMaybes [find (==Initial) states,find (==Fading) states] ++ [head states]
        extractMap list =
          let grouped = groupBy ((==) `on` snd) list
              withKeys = fmap (\l -> (snd $ head l, combineAnimationStates (fst <$> l))) grouped
          in M.fromList withKeys
    mapDyn extractMap allStates

animateState :: MonadWidget t m => CssClass -> CssClass -> CssClass -> Dynamic t AnimationState -> m a -> m (El t, a)
animateState alwaysOn fade appear dynamic inner = do
  let animateCss Standard = [alwaysOn, appear]
      animateCss _ = [alwaysOn, fade]
  classes <- mapDyn animateCss dynamic
  divCssClassDyn classes inner

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

getBody :: MonadWidget t m => m (El t)
getBody = do
  document <- askDocument
  Just body <- liftIO $ documentGetBody document
  wrapElement body

fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight (Right a) = Just a
fromRight _ = Nothing

filterByBehavior func = attachWithMaybe filter
  where filter a b
          | func a = Just b
          | otherwise = Nothing
