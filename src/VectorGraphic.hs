module VectorGraphic
( VectorGraphic(..)
) where

------------------------------------------------------------

import Curve

------------------------------------------------------------

data VectorGraphic = VectorGraphic
  { vg_width  :: Int
  , vg_height :: Int
  , vg_curves :: [Curve]
  }                | Null
  deriving (Show)

