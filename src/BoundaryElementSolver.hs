module BoundaryElementSolver
( solveDerivativeColor
) where

------------------------------------------------------------

import qualified Data.Eigen.Matrix as E
import qualified Data.Eigen.LA     as E
import qualified Data.Vector       as V
import           Data.Complex
import           Control.Monad (forM_)
import           Text.Printf

import Data.Color
import LineSegment

------------------------------------------------------------

solveDerivativeColor :: [LineSegment] -> IO ()
solveDerivativeColor segs = do
  let segments = V.fromList segs
      n        = V.length segments
      dot :: Complex Double -> Complex Double -> Double
      dot (a:+b) (c:+d) = a*c + b*d

      matrixN = E.generate n n $ \i j ->
        let iSeg = segments V.! i
            jSeg = segments V.! j
            dij  = (getMidPoint iSeg) - (getMidPoint jSeg)
            nj   = getNormal jSeg
         in abs $ dot dij nj
      matrixST = E.generate n n $ \i j ->
        let iSeg = segments V.! i
            jSeg = segments V.! j
            uj   = getUnitDirection jSeg
            mi   = getMidPoint iSeg
            sj   = getStartPoint jSeg
            ej   = getEndPoint jSeg
         in (dot (sj-mi) uj) :+ (dot (ej-mi) uj)
      matrixA = E.generate n n $ \i j ->
        let (s :+ t) = E.coeff i j matrixST
            n       = E.coeff i j matrixN
         in atan (t/n) - atan (s/n)
      matrixB = E.generate n n $ \i j ->
        let (s :+ t) = E.coeff i j matrixST
            n       = E.coeff i j matrixN
            a       = E.coeff i j matrixA
            f x     = 0.5 * x * log (n*n + x*x)
         in (s-t) + a * n + (f t - f s)
      vectorCr = E.generate n 1 $ \i j ->
        let Color r _ _ = getBoundaryColor $ segments V.! i in r
      vectorCg = E.generate n 1 $ \i j ->
        let Color _ g _ = getBoundaryColor $ segments V.! i in g
      vectorCb = E.generate n 1 $ \i j ->
        let Color _ _ b = getBoundaryColor $ segments V.! i in b
      matrixPi = E.generate n n $ \i j ->
        if i == j then pi else 0

      vectorEr = E.solve E.HouseholderQR matrixB $ (matrixA - matrixPi) * vectorCr
      vectorEg = E.solve E.HouseholderQR matrixB $ (matrixA - matrixPi) * vectorCg
      vectorEb = E.solve E.HouseholderQR matrixB $ (matrixA - matrixPi) * vectorCb

  forM_ [0..(n-1)] $ \i -> do
    let seg = segments V.! i
        er  = E.coeff i 0 vectorEr
        eg  = E.coeff i 0 vectorEg
        eb  = E.coeff i 0 vectorEb
    updateDerivativeColor seg $ Color er eg eb

