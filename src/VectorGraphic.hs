module VectorGraphic
( VectorGraphic(..)
, getVGBoundingBox
, getDiscretizedSegments
, writeVectorGraphicSegmentsPNG
) where

------------------------------------------------------------

import Data.Complex
import Control.Monad (forM_, when)
import qualified Codec.Picture as J
import qualified Codec.Picture.Types as J

import Data.Color
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
        -- segs <- discretizeCurveUniform x 0.1
        segs <- discretizeCurveLattice x unitSize
        segss <- merge xs
        return $ segs ++ segss
  merge curves

writeVectorGraphicSegmentsPNG :: FilePath -> VectorGraphic -> [LineSegment] -> IO ()
writeVectorGraphicSegmentsPNG fp vg segs = do
  let w = vgWidth vg
      h = vgHeight vg
      toInts :: Complex Double -> (Int, Int)
      toInts (x :+ y) = (truncate x, truncate y)
      to255Color :: Double -> J.Pixel8
      to255Color = max 0 . min 255 . truncate . (*255.0)
      bressenham run rise
        | run  < 0   = [(-x, y) | (x, y) <- bressenham (-run) rise]
        | rise < 0   = [(x, -y) | (x, y) <- bressenham run (-rise)]
        | rise > run = [(x, y)  | (y, x) <- bressenham rise run   ]
        | otherwise = zip [0..run] . map fst $ iterate step (0, run `div` 2)
        where
          step (y, err)
            | err' < 0  = (y + 1, err' + run)
            | otherwise = (y    , err'      )
            where err'  = err - rise
      line (x1, y1) (x2, y2) = [(x1+x, y1+y) | (x, y) <- bressenham (x2-x1) (y2-y1)]

  imgLeft  <- J.createMutableImage h w (J.PixelRGB8 255 255 255)
  imgRight <- J.createMutableImage h w (J.PixelRGB8 255 255 255)
  forM_ segs $ \segment -> do
    let (x0, y0) = toInts . getStartPoint $ segment
        (x1, y1) = toInts . getEndPoint   $ segment
        Color (rl:+rr) (gl:+gr) (bl:+br) = getDebugColor $ segment
        colorLeft  = J.PixelRGB8 (to255Color rl) (to255Color gl) (to255Color bl)
        colorRight = J.PixelRGB8 (to255Color rr) (to255Color gr) (to255Color br)
    forM_ (line (x0, y0) (x1, y1)) $ \(xi, yi) -> when (xi>=0 && xi<w && yi>=0 && yi<h) $ do
      J.writePixel imgLeft  yi xi colorLeft
      J.writePixel imgRight yi xi colorRight

  finalImgLeft  <- J.freezeImage imgLeft
  finalImgRight <- J.freezeImage imgRight
  J.savePngImage (fp++"Left.png")  (J.ImageRGB8 finalImgLeft)
  J.savePngImage (fp++"Right.png") (J.ImageRGB8 finalImgRight)

