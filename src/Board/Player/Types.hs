module Board.Player.Types where

import Rules

import Reflex.Dom
import Control.Monad.Reader
import Data.Maybe

data PlayerExports t = PlayerExports { extractSelectedWorker :: Dynamic t (Maybe WorkerId) }
