module FastMultipoleSolver
( FastMultipoleSolver(..)
, initFMSolver
, calculateMoments
) where

------------------------------------------------------------

import LineSegment

------------------------------------------------------------

data FastMultipoleSolver = FastMultipoleSolver
  { getMaxLevel :: Int
  , getMinLevel :: Int
  , getMaxOrder :: Int
  , getUnitSize :: Double
  }
  deriving (Show)

------------------------------------------------------------

initFMSolver :: Int -> Int -> Int -> Int -> FastMultipoleSolver
initFMSolver w h maxLevel maxOrder =
  let minLevel = if w == h then 0 else 1
      unitSize = (fromIntegral $ max w h) / (2 ** (fromIntegral maxLevel))
   in FastMultipoleSolver maxLevel minLevel maxOrder unitSize

calculateMoments :: FastMultipoleSolver -> [LineSegment] -> IO ()
calculateMoments solver segs = do
  let size = 2^(getMaxLevel solver)

  return ()

