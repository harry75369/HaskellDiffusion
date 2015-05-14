module Geometry.Curve
( Curve(..)
, getCurveBoundingBox
, getCurveLeftColorGidRange
, getCurveRightColorGidRange
, getCurveBlurGidRange
, discretizeCurveUniform
, discretizeCurveLattice
) where

------------------------------------------------------------

import Control.Monad (when)
import Data.Complex
import Text.Printf
import Data.List (sort, nub)

import Data.Color
import Geometry.LineSegment
import Solver.PolynomialSolver (cubForm)

------------------------------------------------------------

data Curve = Curve
  { crGlobalLen     :: Int
  , crLifeTime      :: Int
  , crControlPoints :: [Complex Double]
  , crLeftColors    :: [(Color Double, Int)]
  , crRightColors   :: [(Color Double, Int)]
  , crBlurs         :: [(Double, Int)]
  }
  deriving (Show)

data CubicSpline = CubicSpline
  { csControlPoints :: (Complex Double, Complex Double, Complex Double, Complex Double)
  , csLeftColors    :: [Color Double]
  , csRightColors   :: [Color Double]
  , csBlurs         :: [Double]
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

getCurveBlurGidRange (Curve _ _ _ _ _ bps) = (minimum gids, maximum gids)
  where gids = map snd bps

-- | Discretize the curve uniformly into small line segments according to dt.
--
-- 1) curve is composed of cubic bezier splines, where every four control points make up a spline, and the
--    first and the forth control points lie on the curve, and the forth control point is shared with next spline.
-- 2) every cubic spline is assumed to be equally divided into 1/dt line segments.
-- 3) colors and blurs is indexed by the offset from the start point of the first segment of the first cubic spline.
-- 4) the offset (i.e. global id) ranges from 0 to 10*(N-1)/3, where N is the number of control points.
-- 5) colors and blurs is already sorted by the ascending order of the global id.
discretizeCurveUniform :: Curve -> Double -> IO [LineSegment]
discretizeCurveUniform curve dt = do
  if dt > 0 && dt <= 1.0
     then printf "Discretization dt: %.6f\n" dt
     else error  "Invalid dt"
  let test t dt
        | t >= dt    = test (t-dt) dt
        | otherwise = t
  when (test 0.1 dt /= 0) $
    putStrLn "[WARN] there may be inaccuracy due to error accumulation"

  let leftColors  = piecewizeColors dt $ crLeftColors    curve
      rightColors = piecewizeColors dt $ crRightColors   curve
      blurs       = piecewizeBlurs  dt $ crBlurs         curve
      points      = piecewizePoints dt $ crControlPoints curve
      merge :: [Color Double] -> [Color Double] -> [Double] -> [Complex Double] -> IO [LineSegment]
      merge [lc] [rc] [b] [p] = return []
      merge (lc0:lc1:lcs) (rc0:rc1:rcs) (b0:b1:bs) (p0:p1:ps) = do
        let lc = fmap (/2) (lc0+lc1)
            rc = fmap (/2) (rc0+rc1)
            dc = zipColor lc rc
            c  = lc-rc
            b  = (b0+b1) / 2 :: Double
        seg <- makeLineSegment p0 p1 dc c b
        segs <- merge (lc1:lcs) (rc1:rcs) (b1:bs) (p1:ps)
        return $ seg : segs
      merge l r b p  = do
        putStrLn "[WARN] unmatched line segments properties"
        print l >> print r >> print b >> print p
        return []

  merge leftColors rightColors blurs points

-- | Discretize the curve into line segments which is cut by the lattice.
discretizeCurveLattice :: Curve -> Double -> IO [LineSegment]
discretizeCurveLattice curve unitSize = do
  let dt = 0.1 :: Double
  let leftColors  = piecewizeColors dt $ crLeftColors  curve
      rightColors = piecewizeColors dt $ crRightColors curve
      blurs       = piecewizeBlurs  dt $ crBlurs       curve
      splitList :: [a] -> ([a],  [a])
      splitList list       = (take (n+1) list, drop n list)
        where n = truncate $ 1 / dt
      splinewize :: [Color Double] -> [Color Double] -> [Double] -> [Complex Double] -> [CubicSpline]
      splinewize lcs rcs bs []           = []
      splinewize lcs rcs bs [x]          = []
      splinewize lcs rcs bs [x,y]        = []
      splinewize lcs rcs bs [x,y,z]      = []
      splinewize lcs rcs bs [x,y,z,w]    = [CubicSpline (x,y,z,w) lc rc b]
        where (lc, _) = splitList lcs
              (rc, _) = splitList rcs
              (b,  _) = splitList bs
      splinewize lcs rcs bs (x:y:z:w:xs) = (CubicSpline (x,y,z,w) lc rc b) : css
        where (lc, lcss) = splitList lcs
              (rc, rcss) = splitList rcs
              (b,  bss ) = splitList bs
              css = splinewize lcss rcss bss (w:xs)

  let splines = splinewize leftColors rightColors blurs $ crControlPoints curve
      interpPoint x0 x1 x2 x3 t = (1-t)*(1-t)*(1-t)*x0 + 3*t*(1-t)*(1-t)*x1 + 3*t*t*(1-t)*x2 + t*t*t*x3
      interpColor :: [Color Double] -> Double -> Color Double
      interpColor colors t =
        let n  = length colors
            n0 = floor   $ (fromIntegral $ n-1) * t
            n1 = ceiling $ (fromIntegral $ n-1) * t
            tt = (fromIntegral $ n-1)*t - (fromIntegral n0)
            c0 = colors !! n0
            c1 = colors !! n1
         in fmap (*(1-tt)) c0 + fmap (*tt) c1
      interpBlur :: [Double] -> Double -> Double
      interpBlur blurs t =
        let n  = length blurs
            n0 = floor   $ (fromIntegral $ n-1) * t
            n1 = ceiling $ (fromIntegral $ n-1) * t
            tt = (fromIntegral $ n-1)*t - (fromIntegral n0)
            b0 = blurs !! n0
            b1 = blurs !! n1
         in (1-tt)*b0 + tt*b1
      getIntersectedSegments :: CubicSpline -> IO [LineSegment]
      getIntersectedSegments (CubicSpline (x0:+y0, x1:+y1, x2:+y2, x3:+y3) lcs rcs bs) = genSegments ts
        where xa = (-x0)   + 3*x1 - 3*x2 + x3
              xb = 3*x0    - 6*x1 + 3*x2
              xc = (-3)*x0 + 3*x1
              ya = (-y0)   + 3*y1 - 3*y2 + y3
              yb = 3*y0    - 6*y1 + 3*y2
              yc = (-3)*y0 + 3*y1
              minX = floor   $ minimum [x0, x1, x2, x3] / unitSize
              maxX = ceiling $ maximum [x0, x1, x2, x3] / unitSize
              minY = floor   $ minimum [y0, y1, y2, y3] / unitSize
              maxY = ceiling $ maximum [y0, y1, y2, y3] / unitSize
              xts = [filter (\x -> x>=0 && x<=1) $ cubForm xa xb xc (x0-unitSize*(fromIntegral i)) | i <- [minX..maxX]]
              yts = [filter (\x -> x>=0 && x<=1) $ cubForm ya yb yc (y0-unitSize*(fromIntegral j)) | j <- [minY..maxY]]
              ts  = sort $ nub $ 0 : 1 : (concat xts ++ concat yts)
              genSegments []  = return []
              genSegments [t] = return []
              genSegments (t0:t1:ts) = do
                let p0 = interpPoint (x0:+y0) (x1:+y1) (x2:+y2) (x3:+y3) (t0:+0)
                    p1 = interpPoint (x0:+y0) (x1:+y1) (x2:+y2) (x3:+y3) (t1:+0)
                    lc = interpColor  lcs $ (t0+t1) / 2
                    rc = interpColor  rcs $ (t0+t1) / 2
                    b  = interpBlur   bs  $ (t0+t1) / 2
                    dc = zipColor lc rc
                    c  = lc-rc
                seg <- makeLineSegment p0 p1 dc c b
                segs <- genSegments (t1:ts)

                let aboutZero x = (abs x) < 1e-6
                if aboutZero (t0-t1)
                   then return segs
                   else return $ seg : segs

  fmap concat $ mapM getIntersectedSegments splines

------------------------------------------------------------

-- | Interpolate piecewise colors from discrete color points.
-- Note dt is in range (0,1]
piecewizeColors :: Double -> [(Color Double, Int)] -> [Color Double]
piecewizeColors dt []       = []
piecewizeColors dt [x]      = []
piecewizeColors dt (x:y:[]) =
  let (c0, i0) = x
      (c1, i1) = y
      s = 0.1 / dt
      len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)*s
      blend t = fmap (*(1-t)) c0 + fmap (*t) c1
   in [blend $ i/len | i <- [0..len]]
piecewizeColors dt (x:y:xs) =
   let (c0, i0) = x
       (c1, i1) = y
       s = 0.1 / dt
       len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)*s
       blend t = fmap (*(1-t)) c0 + fmap (*t) c1
    in [blend $ i/len | i <- [0..len-1]] ++ piecewizeColors dt (y:xs)

-- | Interpolate piecewise blurs from discrete blur points
-- Note dt is in range (0,1]
piecewizeBlurs :: Double -> [(Double, Int)] -> [Double]
piecewizeBlurs dt []       = []
piecewizeBlurs dt [x]      = []
piecewizeBlurs dt (x:y:[]) =
  let (b0, i0) = x
      (b1, i1) = y
      s = 0.1 / dt
      len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)*s
      blend t = (1-t) * b0 + t * b1
   in [blend $ i/len | i <- [0..len]]
piecewizeBlurs dt (x:y:xs) =
  let (b0, i0) = x
      (b1, i1) = y
      s = 0.1 / dt
      len = if i0 >= i1 then 1.0 else (fromIntegral $ i1-i0)*s
      blend t = (1-t) * b0 + t * b1
   in [blend $ i/len | i <- [0..len-1]] ++ piecewizeBlurs dt (y:xs)

-- | Interpolate piecewise points from control points
-- Note dt is in range (0,1]
piecewizePoints :: Double -> [Complex Double] -> [Complex Double]
piecewizePoints dt []           = []
piecewizePoints dt [x]          = []
piecewizePoints dt [x,y]        = []
piecewizePoints dt [x,y,z]      = []
piecewizePoints dt (x:y:z:w:[]) = [(fromRational $ (1-t)*(1-t)*(1-t)) * x
                                  +(fromRational $ 3*t*(1-t)*(1-t)) * y
                                  +(fromRational $ 3*t*t*(1-t)) * z
                                  +(fromRational $ t*t*t) * w | t <- [0,(toRational dt)..1.0]]
piecewizePoints dt (x:y:z:w:xs) = [(fromRational $ (1-t)*(1-t)*(1-t)) * x
                                  +(fromRational $ 3*t*(1-t)*(1-t)) * y
                                  +(fromRational $ 3*t*t*(1-t)) * z
                                  +(fromRational $ t*t*t) * w | t <- [0,(toRational dt)..(toRational $ 1.0-dt)]] ++ piecewizePoints dt (w:xs)

