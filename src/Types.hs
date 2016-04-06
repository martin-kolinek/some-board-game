{-# LANGUAGE FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Types where

import Rules
import Common.CssClass

import Reflex.Dom
import Control.Monad.Reader

type UniverseAction = Universe -> Either String Universe

askUniverse :: UniverseReader t m x => m (Dynamic t Universe)
askUniverse = asks extractUniverse

class ContainsUniverse x t | x -> t where
  extractUniverse :: x -> Dynamic t Universe

type UniverseReader t m x = (Reflex t, ContainsUniverse x t, MonadReader x m)
