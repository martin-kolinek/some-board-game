module Player.Building.Types where

import Rules

data TileBuildingType =
  SingleTileBuilding SmallBuildingType |
  BuildingPart LargeBuildingType Direction
  deriving (Eq, Show)

data TileOffset = ExactPosition | NextPosition | OtherPosition

getTileBuildingType :: Direction -> TileOffset -> BuildingDescription -> Maybe TileBuildingType
getTileBuildingType _ ExactPosition (SingleSmallBuildingDesc bt) = Just $ SingleTileBuilding bt
getTileBuildingType _ ExactPosition (DoubleSmallBuildingDesc bt _) = Just $ SingleTileBuilding bt
getTileBuildingType _ NextPosition (DoubleSmallBuildingDesc _ bt) = Just $ SingleTileBuilding bt
getTileBuildingType dir ExactPosition (LargeBuildingDesc bt) = Just $ BuildingPart bt dir
getTileBuildingType dir NextPosition (LargeBuildingDesc bt) = Just $ BuildingPart bt (oppositeDirection dir)
getTileBuildingType _ _ _ = Nothing

oppositeDirection :: Direction -> Direction
oppositeDirection DirectionDown = DirectionUp
oppositeDirection DirectionUp = DirectionDown
oppositeDirection DirectionRight = DirectionLeft
oppositeDirection DirectionLeft = DirectionRight
