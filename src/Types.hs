{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Types where

import Settings.Types

import Rules
import Reflex
import Control.Monad.Reader
import Reflex.Dom
import Data.Semigroup (Semigroup, (<>))
import Control.Arrow ((>>>), Kleisli, runKleisli, Arrow, ArrowApply, returnA)
import Control.Category (Category)

newtype UniverseActionArrow a b = UniverseAction (Kleisli (Either String) a b) deriving (Category, Arrow, ArrowApply)

type UniverseAction = UniverseActionArrow Universe Universe

applyAction :: UniverseAction -> Universe -> Either String Universe
applyAction (UniverseAction f) = runKleisli f

instance Semigroup UniverseAction where
  (UniverseAction a) <> (UniverseAction b) = UniverseAction $ a >>> b

instance Monoid UniverseAction where
  mempty = UniverseAction $ returnA
  mappend = (<>)

class HasUniverseContext t x where
  getUniverseContext :: x -> UniverseContext t

type UniverseWidget t m x = (MonadWidget t m, MonadReader x m, HasUniverseContext t x, EventWriter t UniverseAction m)

data UniverseContext t = UniverseContext { getContextSettings :: Dynamic t PlayerSettings, getContextUniverse :: Dynamic t Universe }

instance HasUniverseContext t (UniverseContext t) where getUniverseContext = id

askUniverseDyn :: UniverseWidget t m x => m (Dynamic t Universe)
askUniverseDyn = asks (getContextUniverse . getUniverseContext)

askPlayerSettings :: UniverseWidget t m x => m (Dynamic t PlayerSettings)
askPlayerSettings = asks (getContextSettings . getUniverseContext)
