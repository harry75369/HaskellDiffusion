module Curve
( Curve(..)
, getCurveBoundingBox
, getCurveLeftColorGidRange
, getCurveRightColorGidRange
, getCurveBlurPointGidRange
, discretizeCurve
) where

------------------------------------------------------------

import Data.Complex
import Data.Color
import LineSegment

------------------------------------------------------------

data Curve = Curve
  { crGlobalLen     :: Int
  , crLifeTime      :: Int
  , crControlPoints :: [Complex Double]
  , crLeftColors    :: [(Color Double, Int)]
  , crRightColors   :: [(Color Double, Int)]
  , crBlurPoints    :: [(Double, Int)]
  }
  deriving (Show)

------------------------------------------------------------

getCurveBoundingBox (Curve _ _ cps _ _ _) = (xMin :+ yMin, xMax :+ yMax)
  where xs = map realPart cps
        ys = map imagPart cps
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys

getCurveLeftColorGidRange (Curve _ _ _ lcs _ _) = (minimum gids, maximum gids)
  where gids = map snd lcs

getCurveRightColorGidRange (Curve _ _ _ _ rcs _) = (minimum gids, maximum gids)
  where gids = map snd rcs

getCurveBlurPointGidRange (Curve _ _ _ _ _ bps) = (minimum gids, maximum gids)
  where gids = map snd bps

-- | Discretize the curve into small line segments so that every segment is wholly contained in a unit square.
--
-- Note in the orignal curve representation
-- 1) curve is composed of cubic bezier splines, where every four control points make up a spline, and the
--    first and the forth control points lie on the curve, and the forth control point is shared with next spline.
-- 2) every cubic spline is assumed to be discretized into 10 line segments.
-- 3) colors and blurs is indexed by the offset from the start point of the first segment of the first cubic spline.
-- 4) the offset (i.e. global id) ranges from 0 to 10*(N-1)/3, where N is the number of control points.
-- 5) colors and blurs is already sorted by the ascending order of the global id.
--
discretizeCurve :: Curve -> Double -> IO [LineSegment]
discretizeCurve curve unitSize = do
  let controlPoints = crControlPoints curve
      leftColors    = crLeftColors curve
      rightColors   = crRightColors curve
      blurPoints    = crBlurPoints curve
      dt = 0.1

  let piecewizeColors []       = []
      piecewizeColors [x]      = []
      piecewizeColors (x:y:xs) =
        let (c0, i0) = x
            (c1, i1) = y
            len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)
            blend t = fmap (*(1-t)) c0 + fmap (*t) c1
         in [blend (fromIntegral i)/len | i <- [0..i1-i0]] ++ piecewizeColors (y:xs)
      piecewizeBlurs []       = []
      piecewizeBlurs [x]      = []
      piecewizeBlurs (x:y:xs) =
        let (b0, i0) = x
            (b1, i1) = y
            len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)
            blend t = (1-t) * b0 + t * b1
         in [blend (fromIntegral i)/len | i <- [0..i1-i0]] ++ piecewizeBlurs (y:xs)
      piecewizePoints []           = []
      piecewizePoints [x]          = []
      piecewizePoints [x,y]        = []
      piecewizePoints [x,y,z]      = []
      piecewizePoints (x:y:z:w:[]) = [(fromRational $ (1-t)*(1-t)*(1-t)) * x
                                     +(fromRational $ 3*t*(1-t)*(1-t)) * y
                                     +(fromRational $ 3*t*t*(1-t)) * z
                                     +(fromRational $ t*t*t) * w | t <- [0,dt..1.0]]
      piecewizePoints (x:y:z:w:xs) = [(fromRational $ (1-t)*(1-t)*(1-t)) * x
                                     +(fromRational $ 3*t*(1-t)*(1-t)) * y
                                     +(fromRational $ 3*t*t*(1-t)) * z
                                     +(fromRational $ t*t*t) * w | t <- [0,dt..1.0-dt]] ++ piecewizePoints (w:xs)

  let leftPiecewiseColors  = piecewizeColors leftColors
      rightPiecewiseColors = piecewizeColors rightColors
      piecewiseBlurs       = piecewizeBlurs  blurPoints
      piecewisePoints      = piecewizePoints controlPoints

  let merge :: [Color Double] -> [Color Double] -> [Double] -> [Complex Double] -> IO [LineSegment]
      merge [lc] [rc] [b] [p] = return []
      merge (lc0:lc1:lcs) (rc0:rc1:rcs) (b0:b1:bs) (p0:p1:ps) = do
        let lc = fmap (/2) (lc0+lc1)
            rc = fmap (/2) (rc0+rc1)
            c  = fmap (fromRational.toRational) (lc-rc) :: Color (Complex Double)
            b  = (b0+b1) / 2 :: Double
        seg <- makeLineSegment p0 p1 c b
        segs <- merge (lc1:lcs) (rc1:rcs) (b1:bs) (p1:ps)
        return $ seg : segs
      merge _ _ _ _  = putStrLn "[WARN] unmatched line segments properties" >> return []

  merge leftPiecewiseColors rightPiecewiseColors piecewiseBlurs piecewisePoints

