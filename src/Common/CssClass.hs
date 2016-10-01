module Common.CssClass where

import Clay hiding (a, b)
import Data.Text

newtype CssClass = CssClass String deriving Show

instance Monoid CssClass where
  mempty = CssClass ""
  mappend (CssClass a) (CssClass b) = CssClass (a ++ " " ++ b)

classSelector :: CssClass -> Refinement
classSelector (CssClass className) = byClass $ pack className
