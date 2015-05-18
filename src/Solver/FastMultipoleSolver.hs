module Solver.FastMultipoleSolver
( FastMultipoleSolver(..)
, initFMSolver
, calculateMoments
) where

------------------------------------------------------------

import           Data.Complex
import           Data.StateVar
import qualified Data.Vector.Mutable as VM
import           Control.Monad (when)

import Data.Color
import Geometry.LineSegment

------------------------------------------------------------

data FastMultipoleSolver = FastMultipoleSolver
  { getMaxLevel :: Int
  , getMinLevel :: Int
  , getMaxOrder :: Int
  , getUnitSize :: Double
  , getMoments  :: VM.IOVector (VM.IOVector CellValue)
  , getLoccoef  :: VM.IOVector (VM.IOVector CellValue)
  }

type CellValue = VM.IOVector (Color (Complex Double))

------------------------------------------------------------

-- | Initialize FMM solver with essential parameters.
initFMSolver :: Int -> Int -> Int -> Int -> IO FastMultipoleSolver
initFMSolver w h maxLevel maxOrder = do
  when (maxLevel <= 0 || maxOrder <=0) $ error "Wrong parameters for FastMultipoleSolver."
  let minLevel = if w == h then 0 else 1
      unitSize = (fromIntegral $ max w h) / (2 ** (fromIntegral maxLevel))
      size     = 2^maxLevel

  let genM = VM.replicateM size genColorM
      genColorM = VM.replicate maxOrder (Color 0 0 0)
  moments <- VM.replicateM size genM
  loccoef <- VM.replicateM size genM

  return $ FastMultipoleSolver maxLevel minLevel maxOrder unitSize moments loccoef

-- | Calculate moments in every cell at the max level.
calculateMoments :: FastMultipoleSolver -> [LineSegment] -> IO ()
calculateMoments solver segs = do
  let size = 2^(getMaxLevel solver)
      unitSize = getUnitSize solver
      funN :: Int -> Complex Double -> LineSegment -> Color (Complex Double)
      funN k zc seg =
        let c = fmap fromReal $ getBoundaryColor seg
            n = getNormal seg
            r = regular (k-1) (z0-zc)
              where z0 = getMidPoint seg
            l = fromReal $ getLength seg
         in fmap (*(n*r*l)) c
      funM :: Int -> Complex Double -> LineSegment -> IO (Color (Complex Double))
      funM k zc seg = do
        c <- fmap (fmap fromReal) $ get (getDerivativeColor seg)
        let r = regular k (z0-zc)
              where z0 = getMidPoint seg
            l = fromReal $ getLength seg
        return $ fmap (*(r*l)) c

  return ()

------------------------------------------------------------

fromInt :: Int -> Complex Double
fromInt = fromIntegral

fromReal :: Double -> Complex Double
fromReal = fromRational.toRational

fact :: Int -> Complex Double
fact k
  | k > 0     = fromInt $ product [1..k]
  | k == 0     = 1
  | otherwise = error "Wrong parameter for factorial!"

regular :: Int -> Complex Double -> Complex Double
regular k z
  | k >= 0     = (-z^k) / (fact k)
  | otherwise = 0

singular :: Int -> Complex Double -> Complex Double
singular k z
  | k > 0     = (fact $ k-1) / (z^k)
  | k == 0     = -(log z)
  | otherwise = 0

