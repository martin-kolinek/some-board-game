{-# LANGUAGE RecursiveDo, FlexibleContexts #-}

module Errors.Dom where

import Rules
import Types
import Common.DomUtil
import Errors.Style

import Reflex.Dom
import Control.Monad
import Data.Map.Strict as M
import Data.List

drawErrors :: (MonadWidget t m, UniverseReader t m x) => Event t UniverseAction -> m ()
drawErrors actions = void $ divCssClass errorContainerClass $ do
  universe <- askUniverse
  let extractError (un, act) = fromLeft $ act un
      attached = attach (current universe) actions
      errorEvents = fmapMaybe extractError attached
      addToMap :: String -> Map Int String -> Map Int String
      addToMap err map = if M.null map then singleton 1 err else M.insert newIndex err map
        where newIndex = fst (findMax map) + 1
      attachIdToEvent :: Reflex t => (Int, Event t ()) -> Event t Int
      attachIdToEvent (id, event) = const id <$> event
      extractEventsFromMap map = leftmost $ attachIdToEvent <$> assocs map
      drawError :: Reflex t => MonadWidget t m => Int -> Dynamic t (AnimationState, String) -> m (Event t ())
      drawError key tuple = do
        err <- snd `mapDyn` tuple
        animState <- fst `mapDyn` tuple
        (_, res) <- animateState (constDyn errorItemClass) (constDyn fadeClass) animState $ do
          el "div" $ dynText err
          el "div" $ buttonSpanCssClass closeButtonWithIconClass (return ())
        return res
      combineWithLast newValue (oldValue, _) = (newValue, newValue M.\\ oldValue)
  rec
    closeEvents <- animated (fromRational 1) allErrors drawError
    allErrors <- foldDyn modifyMap empty addAndRemoveEvents
    additionsDyn <- foldDyn combineWithLast (M.empty, M.empty) (updated allErrors)
    let additions = (M.keys . snd) <$> updated additionsDyn
    timedRemovals <- delay (fromRational 3) additions
    let closeKeyEvents = switch $ extractEventsFromMap <$> current closeEvents
        modifyMap (Left ids) mp = Data.List.foldl' (flip M.delete) mp ids
        modifyMap (Right err) mp = addToMap err mp
        allRemovals = appendEvents (return <$> closeKeyEvents) timedRemovals
        addAndRemoveEvents = leftmost [Right <$> errorEvents, Left <$> allRemovals]
  return ()
