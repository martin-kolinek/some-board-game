module Common.CssClass where

import Clay
import Data.Text
import Reflex.Dom
import Data.String
import Data.Monoid

newtype CssClass = CssClass String deriving Show

instance Monoid CssClass where
  mempty = CssClass ""
  mappend (CssClass a) (CssClass b) = CssClass (a ++ " " ++ b)

classSelector (CssClass className) = byClass $ pack className

fadeClass = CssClass "fade"
