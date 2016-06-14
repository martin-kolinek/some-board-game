module Board.Player.Building.Dom where

import Rules
import Types
import Clay
import Common.DomUtil
import Board.Player.Building.Style

import Reflex.Dom
import Control.Monad
import Data.Text.Lazy

drawBuildingSpace :: (UniverseReader t m x, MonadWidget t m) => PlayerId -> m ()
drawBuildingSpace playerId = do
  universe <- askUniverse
  buildings <- mapDyn (`getBuildingSpace` playerId) universe
  mapDynExtract drawBuildings buildings
  return ()

drawBuildings :: MonadWidget t m => [Building] -> m ()
drawBuildings buildings = void $ divCssClass buildingSpaceClass $
  forM_ buildings $ \building -> do
    let style = styleStringFromCss $ buildingCss building
    elAttr "div" ("style" =: style) $ return ()

buildingCss (Grass position) = backgroundColor green >> positionCss position >> commonBuildingCss
buildingCss (Forest position) = backgroundColor darkgreen >> positionCss position >> commonBuildingCss
buildingCss (Rock position) = backgroundColor gray >> positionCss position >> commonBuildingCss
buildingCss (InitialRoom position) = backgroundColor red >> positionCss position >> commonBuildingCss

commonBuildingCss = width (em 12) >> height (em 12) >> position absolute

positionCss (x, y) = left (em $ fromIntegral x*12) >> top (em $ fromIntegral y*12)
