{-# LANGUAGE RecursiveDo, FlexibleContexts, OverloadedStrings #-}

module Errors.Dom where

import Types
import Common.DomUtil
import Errors.Style

import Reflex.Dom
import Data.Map.Strict as M hiding (map)
import Data.List hiding (map)
import Prelude hiding (map, id)
import Rules
import qualified Data.Text as T

drawErrors :: MonadWidget t m => Dynamic t Universe -> Event t UniverseAction -> m ()
drawErrors universeDyn actions = divAttributeLike errorContainerClass $ do
  let extractError (un, act) = fromLeft $ applyAction act un
      attached = attach (current universeDyn) actions
      errorEvents = fmapMaybe extractError attached
      addToMap :: String -> Map Int String -> Map Int String
      addToMap err map = if M.null map then singleton 1 err else M.insert newIndex err map
        where newIndex = fst (findMax map) + 1
      attachIdToEvent :: Reflex t => (Int, Event t ()) -> Event t Int
      attachIdToEvent (id, event) = const id <$> event
      extractEventsFromMap map = leftmost $ attachIdToEvent <$> assocs map
      drawError :: Reflex t => MonadWidget t m => Int -> Dynamic t String -> m (Event t ())
      drawError _ err = do
        (_, res) <- divAttributeLike' errorItemClass $ do
          el "div" $ dynText (T.pack <$> err)
          el "div" $ buttonSpanCssClass closeButtonWithIconClass (return ())
        return res
      combineWithLast newValue (oldValue, _) = (newValue, newValue M.\\ oldValue)
  rec
    closeEvents <- listWithKey allErrors drawError
    allErrors <- foldDyn modifyMap empty addAndRemoveEvents
    additionsDyn <- foldDyn combineWithLast (M.empty, M.empty) (updated allErrors)
    let additions = (M.keys . snd) <$> updated additionsDyn
    timedRemovals <- delay (fromRational 3) additions
    let closeKeyEvents = switch $ extractEventsFromMap <$> current closeEvents
        modifyMap (Left ids) mp = Data.List.foldl' (flip M.delete) mp ids
        modifyMap (Right err) mp = addToMap err mp
        allRemovals = mappend (return <$> closeKeyEvents) timedRemovals
        addAndRemoveEvents = leftmost [Right <$> errorEvents, Left <$> allRemovals]
  return ()
