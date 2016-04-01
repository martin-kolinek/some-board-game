{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Types where

import Rules

import Reflex.Dom
import Control.Monad.Reader

type UniverseAction = Universe -> Either String Universe

askUniverse :: UniverseReader t m => m (Dynamic t Universe)
askUniverse = ask

type UniverseReader t m = (Reflex t, MonadReader (Dynamic t Universe) m)
