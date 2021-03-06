module VectorGraphic
( VectorGraphic(..)
, getVGBoundingBox
, getDiscretizedSegments
, getBoundarySegments
, writeVectorGraphicSegmentsPNG
, compositeSolverLatticePNG
) where

------------------------------------------------------------

import Data.Complex
import Control.Monad (forM_, when)
import qualified Codec.Picture as J
import qualified Codec.Picture.Types as J

import Data.Color
import Geometry.Curve
import Geometry.LineSegment
import Solver.FastMultipoleSolver

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

-- | Get a clockwise boundary segments loop around the whole vector graphic.
getBoundarySegments :: VectorGraphic -> FastMultipoleSolver -> IO [LineSegment]
getBoundarySegments (VectorGraphic wi hi _ _) fms = do
  let unitSize   = getUnitSize fms
      w = fromIntegral wi
      h = fromIntegral hi
      getColor (1:+0)    = (Color 1 0 0, Color 1 0 0)
      getColor (0:+1)    = (Color 0 1 0, Color 0 1 0)
      getColor ((-1):+0) = (Color 0 0 1, Color 0 0 1)
      getColor (0:+(-1)) = (Color 1 1 0, Color 1 1 0)
      getColor _        = (Color 0 0 0, Color 0 0 0)
      {-
      makeLineSegmentAlong :: Complex Double -> Complex Double -> IO LineSegment
      makeLineSegmentAlong dir start = makeLineSegment start end dc c 0
        where end = start + dir * (unitSize:+0)
              (lc, rc) = getColor dir
              dc  = zipColor lc rc
              c   = fmap (fromRational.toRational) (lc-rc)
  upper  <- mapM (makeLineSegmentAlong $ 1:+0)    [i:+0     | i <- [0,unitSize..(w-unitSize)]]
  right  <- mapM (makeLineSegmentAlong $ 0:+1)    [(w-1):+i | i <- [0,unitSize..(h-unitSize)]]
  bottom <- mapM (makeLineSegmentAlong $ (-1):+0) [i:+(h-1) | i <- reverse [unitSize,2*unitSize..w]]
  left   <- mapM (makeLineSegmentAlong $ 0:+(-1)) [0:+i     | i <- reverse [unitSize,2*unitSize..h]]

  return $ upper ++ right ++ bottom ++ left
  -}
  let makeLineSegmentAlong dir start len = makeLineSegment start end dc c 0
        where end = start + dir * (len:+0)
              (lc, rc) = getColor dir
              dc = zipColor lc rc
              c  = lc - rc
  upper  <- makeLineSegmentAlong (1:+0)    (0:+0) w
  right  <- makeLineSegmentAlong (0:+1)    (w:+0) h
  bottom <- makeLineSegmentAlong ((-1):+0) (w:+h) w
  left   <- makeLineSegmentAlong (0:+(-1)) (0:+h) h

  return $ upper : right : bottom : left : []


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

  imgLeft  <- J.createMutableImage w h (J.PixelRGB8 255 255 255)
  imgRight <- J.createMutableImage w h (J.PixelRGB8 255 255 255)
  forM_ segs $ \segment -> do
    let (x0, y0) = toInts . getStartPoint $ segment
        (x1, y1) = toInts . getEndPoint   $ segment
        Color (rl:+rr) (gl:+gr) (bl:+br) = getDebugColor $ segment
        colorLeft  = J.PixelRGB8 (to255Color rl) (to255Color gl) (to255Color bl)
        colorRight = J.PixelRGB8 (to255Color rr) (to255Color gr) (to255Color br)
    forM_ (line (x0, y0) (x1, y1)) $ \(xi, yi) -> when (xi>=0 && xi<w && yi>=0 && yi<h) $ do
      J.writePixel imgLeft  xi yi colorLeft
      J.writePixel imgRight xi yi colorRight

  finalImgLeft  <- J.freezeImage imgLeft
  finalImgRight <- J.freezeImage imgRight
  J.savePngImage (fp++"Left.png")  (J.ImageRGB8 finalImgLeft)
  J.savePngImage (fp++"Right.png") (J.ImageRGB8 finalImgRight)

compositeSolverLatticePNG :: FilePath -> FastMultipoleSolver -> IO ()
compositeSolverLatticePNG fp solver = do
  Right (J.ImageRGB8 originImg) <- J.readImage (fp++"SegmentsLeft.png")
  let w = J.imageWidth  originImg
      h = J.imageHeight originImg
      s = round $ getUnitSize solver

  let latticeImg = J.generateImage latticeG w h
        where latticeG i j
                | s <= 1             = J.pixelAt originImg i j
                | i == 0 || j == 0
                || i `mod` s + 1 == s
                || j `mod` s + 1 == s = J.PixelRGB8 0 0 0
                | otherwise         = J.pixelAt originImg i j

  J.savePngImage (fp++"Lattice.png") (J.ImageRGB8 latticeImg)

