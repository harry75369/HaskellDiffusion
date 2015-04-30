module VectorGraphic
( VectorGraphic(..)
, getVGBoundingBox
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

------------------------------------------------------------

getVGBoundingBox (VectorGraphic w h curves) = foldl unionBox (fromIntegral w, fromIntegral h, 0, 0) curves
  where unionBox (xMin, yMin, xMax, yMax) curve =
          let (xMinNow, yMinNow, xMaxNow, yMaxNow) = getCurveBoundingBox curve
           in (min xMin xMinNow, min yMin yMinNow, max xMax xMaxNow, max yMax yMaxNow)
