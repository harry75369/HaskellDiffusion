module VectorGraphic
( VectorGraphic(..)
) where

------------------------------------------------------------

import Curve

------------------------------------------------------------

data VectorGraphic = VectorGraphic
  { vgWidth  :: Int
  , vgHeight :: Int
  , vgCurves :: [Curve]
  }
  deriving (Show)

