module Solver.FastMultipoleSolver
( FastMultipoleSolver(..)
, initFMSolver
, calculateMoments
, translateMoments
, calculateLoccoef
) where

------------------------------------------------------------

import           Data.Complex
import           Data.IORef
import           Data.StateVar
import qualified Data.Vector.Mutable as VM
import           Control.Monad (when, forM, forM_)

import Data.Color
import Geometry.LineSegment

------------------------------------------------------------

data FastMultipoleSolver = FastMultipoleSolver
  { getMaxLevel :: Int
  , getMinLevel :: Int
  , getMaxOrder :: Int
  , getUnitSize :: Double
  , getMoments  :: IORef [GridType]
  , getLoccoef  :: IORef [GridType]
  }

type CellType = VM.IOVector (Color (Complex Double))
type GridType = VM.IOVector (VM.IOVector CellType)

------------------------------------------------------------

-- | Initialize FMM solver with essential parameters.
initFMSolver :: Int -> Int -> Int -> Int -> IO FastMultipoleSolver
initFMSolver w h maxLevel maxOrder = do
  when (maxLevel <= 0 || maxOrder <=0) $ error "Wrong parameters for FastMultipoleSolver."
  let minLevel = if w == h then 0 else 1
      unitSize = (fromIntegral $ max w h) / (2 ** (fromIntegral maxLevel))
      size     = 2^maxLevel

  moments <- (fmap (:[]) $ newGrid size maxOrder) >>= newIORef
  loccoef <- (fmap (:[]) $ newGrid size maxOrder) >>= newIORef

  return $ FastMultipoleSolver maxLevel minLevel maxOrder unitSize moments loccoef

-- | Calculate moments in every cell at the max level.
calculateMoments :: FastMultipoleSolver -> [LineSegment] -> IO ()
calculateMoments solver segs = do
  let size = 2^(getMaxLevel solver)
      unitSize = getUnitSize solver
      maxOrder = getMaxOrder solver
      funN :: Complex Double -> LineSegment -> Int -> Color (Complex Double)
      funN zc seg k =
        let c = fmap fromReal $ getBoundaryColor seg
            n = getNormal seg
            r = regular (k-1) (z0-zc)
              where z0 = getMidPoint seg
            l = fromReal $ getLength seg
         in fmap (*(n*r*l)) c
      funM :: Complex Double -> LineSegment -> Int -> IO (Color (Complex Double))
      funM zc seg k = do
        c <- fmap (fmap fromReal) $ get (getDerivativeColor seg)
        let r = regular k (z0-zc)
              where z0 = getMidPoint seg
            l = fromReal $ getLength seg
        return $ fmap (*(r*l)) c

  [moments] <- get $ getMoments solver
  let getMomentsCell :: Int -> Int -> IO CellType
      getMomentsCell = getCell moments

  -- For each line segment, add its impact to the corresponding cell up to maxOrder orders
  forM_ segs $ \seg -> do
    let i = truncate $ (realPart $ getMidPoint seg) / unitSize
        j = truncate $ (imagPart $ getMidPoint seg) / unitSize

    when (i>=0 && i<size && j>=0 && j<size) $ do
      let zc = fromCellIdx unitSize i j

      cell <- getMomentsCell i j
      forM_ [1..maxOrder] $ \k -> do
        v <- VM.read cell (k-1) :: IO (Color (Complex Double))
        let n = funN zc seg k :: Color (Complex Double)
        m <- funM zc seg k :: IO (Color (Complex Double))
        VM.write cell (k-1) (v+n-m)

-- | Translate moments from the max to the min level between hierarchies.
translateMoments :: FastMultipoleSolver -> IO ()
translateMoments solver = do
  let maxOrder = getMaxOrder solver
      maxLevel = getMaxLevel solver
      minLevel = getMinLevel solver

  when (minLevel<0 || maxLevel<=0 || minLevel>maxLevel) $
    error "Wrong parameters for FastMultipoleSolver."

  let reduceM :: GridType -> Int -> Double -> IO GridType
      reduceM moments size unitSize = do
        let newSize     = size `div` 2
            newUnitSize = unitSize * 2

        newMoments <- newGrid newSize maxOrder

        forM_ [0..(newSize-1)] $ \i -> do
          forM_ [0..(newSize-1)] $ \j -> do
            newCell <- getCell newMoments i j

            let zcNew = fromCellIdx newUnitSize i j
                translateFrom i j = do
                  cell <- getCell moments i j
                  let zcDiff = (fromCellIdx unitSize i j) - zcNew
                  forM_ [0..(maxOrder-1)] $ \k -> do
                    terms <- forM [0..k] $ \t -> do
                      a <- VM.read cell t
                      return $ fmap (*(regular (k+1-t) zcDiff)) a
                    VM.write newCell k (negate.sum $ terms)

            translateFrom (2*i)   (2*j)
            translateFrom (2*i+1) (2*j)
            translateFrom (2*i)   (2*j+1)
            translateFrom (2*i+1) (2*j+1)

        return newMoments

  [moments] <- get $ getMoments solver
  let iterateM :: Int -> Int -> Double -> GridType -> IO [GridType]
      iterateM level size unitSize moments = do
        if level <= minLevel
           then return [moments]
           else do
             newMoments <- reduceM moments size unitSize
             momentss <- iterateM (level-1) (div size 2) (unitSize*2) newMoments
             return $ moments : momentss

  let size = 2^(getMaxLevel solver)
      unitSize = getUnitSize solver
  momentsHierarchy <- iterateM maxLevel size unitSize moments
  (getMoments solver) $= momentsHierarchy

-- | Calculate local coefficients in every cell at the max level.
calculateLoccoef :: FastMultipoleSolver -> IO ()
calculateLoccoef solver = do
  let maxLevel = getMaxLevel solver
      minLevel = getMinLevel solver
      maxOrder = getMaxOrder solver

  when (minLevel<0 || maxLevel<=0 || minLevel>maxLevel) $
    error "Wrong parameters for FastMultipoleSolver."

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

newGrid :: Int -> Int -> IO GridType
newGrid size maxOrder = do
  let genRow  = VM.replicateM size genCell
      genCell = VM.replicate maxOrder (Color 0 0 0)
  VM.replicateM size genRow

getCell :: GridType -> Int -> Int -> IO CellType
getCell grid i j = VM.read grid i >>= (flip VM.read) j

fromCellIdx :: Double -> Int -> Int -> Complex Double
fromCellIdx unitSize i j = (fromIntegral i + 0.5)*unitSize :+ (fromIntegral j + 0.5)*unitSize
