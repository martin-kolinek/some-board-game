{-# LANGUAGE OverloadedStrings #-}
module Common.CssClass where

import Clay hiding (a, b)
import Data.Text
import Data.Monoid

newtype CssClass = CssClass Text deriving Show

instance Monoid CssClass where
  mempty = CssClass ""
  mappend (CssClass a) (CssClass b) = CssClass (a <> " " <> b)

classSelector :: CssClass -> Refinement
classSelector (CssClass className) = byClass $ className
