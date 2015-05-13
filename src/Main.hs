module Main
( main
) where

------------------------------------------------------------

import Control.Monad (mapM_, foldM, when)
import Data.List (sort, nub)
import System.Environment (getArgs)
import Options.Applicative
import System.TimeIt (timeItT)
import System.FilePath (takeBaseName)
import Text.Printf
import Data.Complex
import Data.StateVar

import Data.Color
import Parsers.XMLParser
import OpenGL.Window
import OpenGL.Shader
import VectorGraphic
import Curve
import LineSegment
import FastMultipoleSolver
import BoundaryElementSolver

------------------------------------------------------------

data Args = Args
  { vectorGraphicFile  :: FilePath
  , debug              :: Bool
  , vertexShaderFile   :: FilePath
  , geometryShaderFile :: FilePath
  , fragmentShaderFile :: FilePath
  , maxHierarchyLevel  :: Int
  , maxExpansionOrder  :: Int
  }
  deriving (Show)

args :: Parser Args
args = Args
  <$> strArgument (value "" <> metavar "Vector graphic file")
  <*> switch (short 'd' <> long "debug" <> help "Enable debug")
  <*> strOption (short 'v' <> long "vertex" <> help "Vertex shader file" <> value "shaders/vertex.glsl" <> metavar "FILE")
  <*> strOption (short 'g' <> long "geometry" <> help "Geometry shader file" <> value "" <> metavar "FILE")
  <*> strOption (short 'f' <> long "fragment" <> help "Fragment shader file" <> value "shaders/fragment.glsl" <> metavar "FILE")
  <*> option auto (short 'l' <> long "level" <> help "Max hierarchy level" <> value 10 <> metavar "NUM")
  <*> option auto (short 'o' <> long "order" <> help "Max expansion order" <> value 4 <> metavar "NUM")

------------------------------------------------------------

main :: IO ()
main = execParser extraArgs >>= processArgs
  where shortDesc = "Diffusion Curves in Haskell, Chaoya Li <chaoya@chaoya.info> (C) 2015"
        extraArgs = info (helper <*> args) (fullDesc <> header shortDesc)
        processArgs (Args [] _ v g f _ _) = runOpenGL (ShaderContainer v g f) (Nothing, Nothing)
        processArgs (Args fp False v g f l o) = fmap (makeFMSolver l o) (profileParse $! parseXMLFile fp) >>= runOpenGL (ShaderContainer v g f)
        processArgs (Args fp True _ _ _ l o) = fmap (makeFMSolver l o) (profileParse $! parseXMLFile fp) >>= debugVectorGraphic
        profileParse :: IO a -> IO a
        profileParse = makeProfiler "Parsing time: %6.2fs\n"
        makeFMSolver :: Int -> Int -> Maybe VectorGraphic -> (Maybe VectorGraphic, Maybe FastMultipoleSolver)
        makeFMSolver l o Nothing = (Nothing, Nothing)
        makeFMSolver l o (Just vg) = (Just vg, solver)
          where solver = if l>0 && o>0
                            then Just $ initFMSolver (vgWidth vg) (vgHeight vg) l o
                            else Nothing

runOpenGL :: ShaderContainer -> (Maybe VectorGraphic, Maybe FastMultipoleSolver) -> IO ()
runOpenGL shaders (Just vg, Just solver) = do
  preprocessVectorGraphic solver vg
  newWindow (vgWidth vg) (vgHeight vg) "Vector Graphics" shaders Nothing >>= runWindow
runOpenGL shaders (_, _) = newWindow 800 600 "Vector Graphics" shaders Nothing >>= runWindow

preprocessVectorGraphic :: FastMultipoleSolver -> VectorGraphic -> IO [LineSegment]
preprocessVectorGraphic solver vg = do
  printf "Begin to prepreocess the vector graphic...\n"

  let unitSize = getUnitSize solver
  printf "Unit size: %.2f\n" unitSize

  let profileDiscretization = makeProfiler "Curve discretization time: %6.2fs\n"
  segments <- profileDiscretization $! getDiscretizedSegments vg solver

  let nSegs = length segments
  printf "Number of line segments: %d\n" nSegs
  nValids <- foldM (\accum seg -> do
        flag <- debugSegment unitSize seg
        return $ accum+flag) 0 segments
  printf "Percentage of valid segment: %.2f (%d/%d)\n"
    ((fromIntegral nValids)/(fromIntegral $ nSegs) :: Double) nValids nSegs

  boundarySegments <- getBoundarySegments vg solver
  let allSegments = boundarySegments ++ segments
  let fileName = takeBaseName $ vgFilePath vg
  writeVectorGraphicSegmentsPNG (fileName ++ "Segments") vg allSegments

  let profileBEM  = makeProfiler "BEM solving time: %6.2fs\n"
  profileBEM $! solveDerivativeColor allSegments >> mapM_ debugSegmentColor allSegments

  calculateMoments solver allSegments

  return allSegments

makeProfiler :: String -> IO a -> IO a
makeProfiler format act = do
  (t, a) <- timeItT act
  printf format t >> return a

------------------------------------------------------------

debugVectorGraphic :: (Maybe VectorGraphic, Maybe FastMultipoleSolver) -> IO ()
debugVectorGraphic (Nothing, _)  = putStrLn "Please provide a valid vector graphic file."
debugVectorGraphic (_, Nothing)  = putStrLn "Wrong solver parameters."
debugVectorGraphic (Just vg, Just solver) = do
  let curves   = vgCurves   vg
      width    = vgWidth    vg
      height   = vgHeight   vg
      filePath = vgFilePath vg
      seesee curve = (crGlobalLen curve
                     ,crLifeTime curve
                     ,length . crControlPoints $ curve
                     ,getCurveLeftColorGidRange curve
                     ,getCurveRightColorGidRange curve
                     ,getCurveBlurGidRange curve)
      (xMin :+ yMin, xMax :+ yMax) = getVGBoundingBox vg
      nTotalCPs = foldl (\n curve -> n + (length.crControlPoints $ curve)) 0 curves

  printf "File: %s\n" filePath
  -- print vg
  printf "Size: (%d, %d)\n" width height
  printf "Number of curves: %d\n" (length curves)
  printf "Number of total control points: %d\n" nTotalCPs
  printf "Bounding box: (%.2f, %.2f, %.2f, %.2f)\n" xMin yMin xMax yMax
  -- mapM_ print $ sort $ nub $ map seesee curves

  preprocessVectorGraphic solver vg
  return ()

debugSegment :: Double -> LineSegment -> IO Int
debugSegment unitSize (LineSegment (sx:+sy) (ex:+ey) _ l _ _ _ _ _ _) = do
  let aboutZero x = (abs x) < 1e-6
  let onBoundary pos
        | pos < 0                          = onBoundary (-pos)
        | pos >= unitSize                   = onBoundary (pos-unitSize)
        | (pos < unitSize) && aboutZero pos = True
        | aboutZero (pos-unitSize)         = True
        | otherwise                        = False
      toInt True = 1 :: Int
      toInt False = 0 :: Int
      du = 1e-6
  let si = (floor $ (sx+du) / unitSize) :: Int
      sj = (floor $ (sy+du) / unitSize) :: Int
      ei = (floor $ (ex+du) / unitSize) :: Int
      ej = (floor $ (ey+du) / unitSize) :: Int
      bsx = onBoundary sx
      bsy = onBoundary sy
      bex = onBoundary ex
      bey = onBoundary ey
      inSameUnit :: Bool
      inSameUnit
        | (bsx || bsy) && not (bex || bey) = (si==ei && sj==ej) || (si==ei+1 && sj==ej) || (si==ei && sj==ej+1) || (si==ei+1 && sj==ej+1)
        | not (bsx || bsy) && (bex || bey) = (si==ei && sj==ej) || (si+1==ei && sj==ej) || (si==ei && sj+1==ej) || (si+1==ei || sj+1==ej)
        | (bsx || bsy) && (bex || bey)  = (si==ei && sj==ej)
                                     || (si==ei+1 && sj==ej) || (si==ei && sj==ej+1)
                                     || (si+1==ei && sj==ej) || (si==ei && sj+1==ej)
                                     || (si+1==ei && sj+1==ej) || (si==ei+1 && sj==ej+1)
                                     || (si+1==ei && sj==ej+1) || (si==ei+1 && sj+1==ej)
        | otherwise                  = (si==ei && sj==ej)
      flag = toInt inSameUnit
  when (not inSameUnit) $
    printf "(%.2f,%.2f) -> (%.2f,%.2f), %d %d %d %d, len = %.2f, (%d,%d) -> (%d,%d), %d\n"
      sx sy ex ey (toInt bsx) (toInt bsy) (toInt bex) (toInt bey) l si sj ei ej flag
  return flag

debugSegmentColor :: LineSegment -> IO ()
debugSegmentColor (LineSegment _ _ _ _ _ _ _ c e _) = do
  let printColor :: Color Double -> IO ()
      printColor (Color r g b) = printf "Color %.2f %.2f %.2f" r g b
  ec <- get e
  printColor c >> printf " " >> printColor ec >> printf "\n"

