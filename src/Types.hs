{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Types where

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

class HasUniverseDyn t x where
  getUniverseDyn :: x -> Dynamic t Universe

type UniverseWidget t m x = (MonadWidget t m, MonadReader x m, HasUniverseDyn t x)

