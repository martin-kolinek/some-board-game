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

data AnimationState a = Initial a | Standard a | Fading a

fromAnimationState (Initial a) = a
fromAnimationState (Standard a) = a
fromAnimationState (Fading a) = a

isInitial (Initial _) = True
isInitial _ = False

isFading (Initial _) = True
isFading _ = False

animateList :: MonadWidget t m => Ord a => NominalDiffTime -> Dynamic t [a] -> m (Dynamic t (M.Map a (AnimationState a)))
animateList time input = do
  let findInitials nonDelayedList delayed1List = Initial <$> (nonDelayedList \\ delayed1List)
      findFading delayed1List delayedTimeList = Fading <$> (delayedTimeList \\ delayed1List)
      findStandard nonDelayedList delayed1List delayedTimeList = Standard <$> (delayed1List `intersect` (nonDelayedList `union` delayedTimeList))
  delayed1 <- delay 0.0001 (updated input)
  delayed1Dyn <- holdDyn [] delayed1
  delayedTime <- delay time (updated input)
  delayedTimeDyn <- holdDyn [] delayedTime
  initials <- combineDyn findInitials input delayed1Dyn
  combined1 <- combineDyn (,) input delayed1Dyn
  combined2 <- combineDyn (,) combined1 delayedTimeDyn
  standard <- mapDyn (uncurry (uncurry findStandard)) combined2
  fading <- combineDyn findFading delayed1Dyn delayedTimeDyn
  allStates <- mconcatDyn [initials, standard, fading]
  let combineAnimationStates states = head $
        catMaybes [find isInitial states,find isFading states] ++ [head states]
      extractMap list =
        let grouped = groupBy ((==) `on` fromAnimationState) list
            withKeys = fmap (\l -> (fromAnimationState $ head l, combineAnimationStates l)) grouped
        in M.fromList withKeys
  mapDyn extractMap allStates

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
