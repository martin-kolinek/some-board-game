{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Player.Types where

import Rules
import Types

import Reflex.Dom
import Control.Monad.Reader
import Control.Arrow (Kleisli(..))

class HasPlayerId x where
  getPlayerId :: x -> PlayerId

data PlayerContext t = PlayerContext { getInnerUniverseContext :: UniverseContext t, getContextPlayerId :: PlayerId }

instance HasPlayerId (PlayerContext t) where getPlayerId = getContextPlayerId
instance HasUniverseContext t (PlayerContext t) where getUniverseContext = getInnerUniverseContext

type PlayerWidget t m x = (UniverseWidget t m x, HasPlayerId x)

askPlayerId :: PlayerWidget t m x => m PlayerId
askPlayerId = asks getPlayerId

type PlayerAction = PlayerId -> Universe -> Either String Universe

makeUniverseAction :: PlayerId -> PlayerAction -> UniverseAction
makeUniverseAction plId act = UniverseAction $ Kleisli $ act plId

tellPlayerAction :: (PlayerWidget t m x) => Event t PlayerAction -> m ()
tellPlayerAction act = do
  plId <- askPlayerId
  tellEvent $ makeUniverseAction plId <$> act
