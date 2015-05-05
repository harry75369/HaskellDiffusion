module LineSegment
( LineSegment(..)
, makeLineSegment
, updateDerivativeColor
) where

------------------------------------------------------------

import Data.Complex
import Data.Color
import Data.IORef
import Data.StateVar

------------------------------------------------------------

data LineSegment = LineSegment
  { getStartPoint      :: Complex Double
  , getEndPoint        :: Complex Double
  , getMidPoint        :: Complex Double
  , getLength          :: Double
  , getUnitDirection   :: Complex Double
  , getNormal          :: Complex Double
  , getBoundaryColor   :: Color (Complex Double)
  , getDerivativeColor :: IORef (Color (Complex Double))
  , getBlur            :: Double
  }

instance Show LineSegment where
  show (LineSegment s e _ _ _ _ _ _ _) = show s ++ " -> " ++ show e

------------------------------------------------------------

makeLineSegment :: Complex Double -> Complex Double -> Color (Complex Double) -> Double -> IO LineSegment
makeLineSegment s e c b = do
  let m = (s + e) / 2
      l = magnitude $ e - s
      u = makeUnit  $ e - s
        where makeUnit :: Complex Double -> Complex Double
              makeUnit 0 = 0
              makeUnit d = d / (l :+ 0)
      n = u * (0 :+ 1)
  d <- newIORef (Color 0 0 0 :: Color (Complex Double))
  return $ LineSegment s e m l u n c d b

updateDerivativeColor :: LineSegment -> Color (Complex Double) -> IO ()
updateDerivativeColor seg c = (getDerivativeColor seg) $= c

