module VectorGraphic
( VectorGraphic(..)
, getVGBoundingBox
, getDiscretizedSegments
) where

------------------------------------------------------------

import Data.Complex
import Curve
import LineSegment
import FastMultipoleSolver

------------------------------------------------------------

data VectorGraphic = VectorGraphic
  { vgWidth    :: Int
  , vgHeight   :: Int
  , vgCurves   :: [Curve]
  , vgFilePath :: FilePath
  }
  deriving (Show)

------------------------------------------------------------

getVGBoundingBox :: VectorGraphic -> (Complex Double, Complex Double)
getVGBoundingBox (VectorGraphic w h curves _) = foldl unionBox ((fromIntegral w) :+ (fromIntegral h), 0) curves
  where unionBox (xMin :+ yMin, xMax :+ yMax) curve =
          let (xMinNow :+ yMinNow, xMaxNow :+ yMaxNow) = getCurveBoundingBox curve
           in ((min xMin xMinNow) :+ (min yMin yMinNow), (max xMax xMaxNow) :+ (max yMax yMaxNow))

getDiscretizedSegments :: VectorGraphic -> FastMultipoleSolver -> IO [LineSegment]
getDiscretizedSegments (VectorGraphic w h curves _) fms = do
  let unitSize = getUnitSize fms
      merge :: [Curve] -> IO [LineSegment]
      merge [] = return []
      merge (x:xs) = do
        segs <- discretizeCurve x unitSize
        segss <- merge xs
        return $ segs ++ segss
  merge curves
