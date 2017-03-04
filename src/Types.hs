{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Types where

import Rules

type UniverseAction = Universe -> Either String Universe
