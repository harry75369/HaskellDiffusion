module Solver.FastMultipoleSolver
( FastMultipoleSolver(..)
, initFMSolver
, calculateMoments
, translateMoments
, calculateLoccoef
, saveImage
) where

------------------------------------------------------------

import           Data.Complex
import           Data.IORef
import           Data.StateVar
import qualified Data.Vector.Mutable as VM
import           Control.Monad (when, forM, forM_)
import           Text.Printf
import qualified Codec.Picture as J
import qualified Codec.Picture.Types as J

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
  loccoef <- newIORef []

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

    -- TODO: filter line segments according to image plane, rather than the lattice
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

  printf "Moments hierarchy size: %d\n" (length momentsHierarchy)
  (getMoments solver) $= momentsHierarchy

-- | Calculate local coefficients in every cell at the max level.
calculateLoccoef :: FastMultipoleSolver -> IO ()
calculateLoccoef solver = do
  let maxLevel = getMaxLevel solver
      minLevel = getMinLevel solver
      maxOrder = getMaxOrder solver

  when (minLevel<0 || maxLevel<=0 || minLevel>maxLevel) $
    error "Wrong parameters for FastMultipoleSolver."

  let findProperNeighbors :: Int -> Int -> Int -> [(Int,Int)]
      findProperNeighbors size i j =
        let isNeighbor :: (Int,Int) -> (Int,Int) -> Bool
            isNeighbor (x0,y0) (x1,y1) = abs (x0-x1) < 2 && abs (y0-y1) < 2
            parentNeighbor :: Int -> [Int]
            parentNeighbor x =
              let parent = x `quot` 2
                  low    = max 0 $ parent - 1
                  upper  = min ((size `quot` 2) -1) $ parent + 1
               in [2*i+j | i <- [low..upper], j <- [0,1]]
         in [(nx,ny) | nx <- (parentNeighbor i), ny <- (parentNeighbor j),
                       not $ isNeighbor (i,j) (nx,ny)]
  {-
  let findProperNeighbors :: Int -> Int -> Int -> [(Int,Int)]
      findProperNeighbors size i j =
        let isNeighbor :: (Int,Int) -> (Int,Int) -> Bool
            isNeighbor (x0,y0) (x1,y1) = abs (x0-x1) < 2 && abs (y0-y1) < 2
            parent (i, j) = (i `div` 2, j `div` 2)
         in [(nx,ny) | nx <- [0..(size-1)], ny <- [0..(size-1)],
                       not $ isNeighbor (i,j) (nx,ny),
                       isNeighbor (parent (i,j)) (parent (nx,ny))]
  -}

  let momentsToLoccoef :: (GridType, Double) -> IO GridType
      momentsToLoccoef (moments, unitSize) = do
        let size = VM.length moments

        loccoef <- newGrid size maxOrder

        forM_ [0..(size-1)] $ \i -> do
          forM_ [0..(size-1)] $ \j -> do
            loccell <- getCell loccoef i j
            let zl = fromCellIdx unitSize i j
                neighbors = findProperNeighbors size i j
            forM_ neighbors $ \(mi,mj) -> do
              momcell <- getCell moments mi mj
              let zc    = fromCellIdx unitSize mi mj
                  zDiff = zl - zc
              forM_ [0..(maxOrder-1)] $ \t -> do
                terms <- forM [0..(maxOrder-1)] $ \k -> do
                  a <- VM.read momcell k
                  return $ fmap (*(singular (k+t+1) zDiff)) a
                let s = if even t then negate else id
                    p :: Complex Double -> Complex Double
                    p = (*) (0.5 / pi :: Complex Double)
                VM.write loccell t (s.(fmap p).sum $ terms)

        return loccoef
      loccoefToLoccoef :: ((GridType, Double), (GridType, Double)) -> IO ()
      loccoefToLoccoef ((src, srcUnitSize), (dst, dstUnitSize)) = do
        let srcSize = VM.length src
            dstSize = VM.length dst

        when (srcSize*2/=dstSize || dstUnitSize*2/=srcUnitSize) $
          error "Wrong parameters for local coefficients translation."

        forM_ [0..(srcSize-1)] $ \i -> do
          forM_ [0..(srcSize-1)] $ \j -> do
            srcCell <- getCell src i j
            let zl = fromCellIdx srcUnitSize i j
                translateTo i j = do
                  dstCell <- getCell dst i j
                  let zDiff = (fromCellIdx dstUnitSize i j) - zl
                  forM_ [0..(maxOrder-1)] $ \t -> do
                    terms <- forM [t..(maxOrder-1)] $ \k -> do
                      l <- VM.read srcCell k
                      return $ fmap (*(regular (k-t) zDiff)) l
                    v <- VM.read dstCell t
                    VM.write dstCell t (v + (negate.sum $ terms))

            translateTo (2*i)   (2*j)
            translateTo (2*i+1) (2*j)
            translateTo (2*i)   (2*j+1)
            translateTo (2*i+1) (2*j+1)

  -- Get moments hierarchy from minLevel to maxLevel,
  -- and unit sizes from minLevel to maxLevel
  momentsHierarchy <- fmap reverse $ get (getMoments solver)
  let unitSizes = reverse $ take (maxLevel-minLevel+1) $ iterate (*2) $ getUnitSize solver

  let work :: IO ()
      work
        | maxLevel == 1 = do
          let [moments] = drop (if minLevel == 0 then 1 else 0) momentsHierarchy
          (getMoments solver) $= [moments]
          (getLoccoef solver) $= []
        | maxLevel == 2 = do
          let [moments] = drop (if minLevel == 0 then 2 else 1) momentsHierarchy
          (getMoments solver) $= [moments]
          (getLoccoef solver) $= []
        | otherwise    = do
          let moments = drop (if minLevel == 0 then 2 else 1) momentsHierarchy
              usizes  = drop (if minLevel == 0 then 2 else 1) unitSizes

          -- Calculate local coefficients from level 2 to maxLevel
          loccoef <- mapM momentsToLoccoef $ zip moments usizes
          let l = zip loccoef usizes
          mapM_ loccoefToLoccoef $ zip l (tail l)
          (getMoments solver) $= []
          (getLoccoef solver) $= [last loccoef]

  work

saveImage :: FastMultipoleSolver -> FilePath -> Int -> Int -> IO ()
saveImage solver filePath width height = do
  printf "Saving an %dx%d image: %s\n" width height (filePath++".png")

  let maxLevel = getMaxLevel solver
      size     = 2^maxLevel
      unitSize = getUnitSize solver
      maxOrder = getMaxOrder solver
  moments <- get (getMoments solver)
  loccoef <- get (getLoccoef solver)
  imgGrid <- newGrid (max width height) 1

  let to255Color :: Color (Complex Double) -> J.PixelRGB8
      to255Color (Color (r:+_) (g:+_) (b:+_)) =
        let f :: Double -> J.Pixel8
            f = max 0 . min 255 . truncate . (*255.0)
         in J.PixelRGB8 (f r) (f g) (f b)

  let work [moments] [] = do
        when (size/=(VM.length moments)) $
          error "Invalid moments grid size for saving image."

        printf "Calculating image using moments...\n"

        let p :: Complex Double -> Complex Double
            p = (*) (0.5 / pi :: Complex Double)

        forM_ [0..(width-1)] $ \i -> do
          forM_ [0..(height-1)] $ \j -> do
            let z = fromCellIdx 1.0 i j
            pixel <- getCell imgGrid i j
            printf "Calculating pixel (%d,%d)\n" i j

            forM_ [0..(size-1)] $ \mi -> do
              forM_ [0..(size-1)] $ \mj -> do
                let zc    = fromCellIdx unitSize mi mj
                    zDiff = z - zc
                cell <- getCell moments mi mj
                terms <- forM [0..(maxOrder-1)] $ \k -> do
                  a <- VM.read cell k
                  return $ fmap (*(singular (k+1) zDiff)) a
                v <- VM.read pixel 0
                VM.write pixel 0 (v + ((fmap p).sum $ terms))

        image <- J.withImage width height $ \i j -> do
          pixel <- getCell imgGrid i j
          value <- VM.read pixel 0
          return $ to255Color value

        J.savePngImage (filePath++".png") (J.ImageRGB8 image)

      work [] [loccoef] = do
        when (2^maxLevel/=(VM.length loccoef)) $
          error "Invalid loccoef grid size for saving image."

        printf "Calculating image using local coefficients...\n"

        forM_ [0..(width-1)] $ \i -> do
          forM_ [0..(height-1)] $ \j -> do
            let z  = fromCellIdx 1.0 i j
                li = truncate $ (realPart z) / unitSize
                lj = truncate $ (imagPart z) / unitSize
                zl = fromCellIdx unitSize li lj
                zDiff = z - zl

            pixel   <- getCell imgGrid i j
            loccell <- getCell loccoef li lj
            terms <- forM [0..(maxOrder-1)] $ \t -> do
              a <- VM.read loccell t
              return $ fmap (*(regular t zDiff)) a
            let color = sum terms
            VM.write pixel 0 color

            let Color (r:+_) (g:+_) (b:+_) = color
            printf "Calculating pixel (%d,%d): Color %.2f %.2f %.2f\n" i j r g b

        image <- J.withImage width height $ \i j -> do
          pixel <- getCell imgGrid i j
          value <- VM.read pixel 0
          return $ to255Color value

        J.savePngImage (filePath++".png") (J.ImageRGB8 image)

      work _ _ = do
        error "Invalid solver inner states for saving image."

  work moments loccoef

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
