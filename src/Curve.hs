module Curve
( Curve(..)
) where

------------------------------------------------------------

------------------------------------------------------------

data Curve = Curve
  { crGlobalLen     :: Int
  , crLifeTime      :: Int
  , crControlPoints :: [(Double, Double)]
  , crLeftColors    :: [(Int, Int, Int, Int)]
  , crRightColors   :: [(Int, Int, Int, Int)]
  , crBlurPoints    :: [(Int, Int)]
  }
  deriving (Show)

