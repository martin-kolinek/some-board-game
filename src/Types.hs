module Types where

import Rules

type UniverseAction = Universe -> Either String Universe
