module Board.Player.Types where

import Rules

import Reflex.Dom

data PlayerExports t = PlayerExports { extractSelectedWorker :: Dynamic t (Maybe WorkerId) }
