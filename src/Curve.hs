module Curve
( Curve(..)
, getCurveBoundingBox
, getCurveLeftColorGidRange
, getCurveRightColorGidRange
, getCurveBlurPointGidRange
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

------------------------------------------------------------

getCurveBoundingBox (Curve _ _ cps _ _ _) = (xMin, yMin, xMax, yMax)
  where xs = map fst cps
        ys = map snd cps
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys

getCurveLeftColorGidRange (Curve _ _ _ lcs _ _) = (minimum gids, maximum gids)
  where gids = map (\(_,_,_,gid)->gid) lcs

getCurveRightColorGidRange (Curve _ _ _ _ rcs _) = (minimum gids, maximum gids)
  where gids = map (\(_,_,_,gid)->gid) rcs

getCurveBlurPointGidRange (Curve _ _ _ _ _ bps) = (minimum gids, maximum gids)
  where gids = map snd bps
