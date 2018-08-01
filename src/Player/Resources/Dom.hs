module Player.Resources.Dom where

import Rules
import Common.CssClass
import Player.Resources.Style
import Common.DomUtil

import Reflex.Dom
import Data.Monoid ((<>))
import Data.Text as T
import Control.Monad (replicateM_, forM_)

drawResourcesDyn :: MonadWidget t m => Dynamic t Resources -> m ()
drawResourcesDyn res = dyn_ $ drawResources <$> res

drawResources :: MonadWidget t m => Resources -> m ()
drawResources res = divAttributeLike resourceHolderClass $
  forM_ resourceTypes $ \(func, cls) -> 
    drawResource res func cls

drawResource :: MonadWidget t m => Resources -> (Resources -> Int) -> CssClass -> m ()
drawResource res func cls = do
  let number = func res
  if number == 0 then return ()
  else divAttributeLike resourceLineClass $
    if number > 3 then do
      divAttributeLike (cls <> resourceIconClass) $ return ()
      text $ T.pack $ show number
    else do
      replicateM_ number $ divAttributeLike (cls <> resourceIconClass) $ return ()

resourceTypes :: [(Resources -> Int, CssClass)]
resourceTypes = [(getWoodAmount, woodIconClass),
                 (getStoneAmount, stoneIconClass),
                 (getGoldAmount, goldIconClass),
                 (getIronAmount, ironIconClass),
                 (getWheatAmount, wheatIconClass),
                 (getPotatoAmount, potatoIconClass),
                 (getMoneyAmount, moneyIconClass),
                 (getFoodAmount, foodIconClass)]
