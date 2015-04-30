module Main
( main
) where

------------------------------------------------------------

import Control.Monad (mapM_)
import Data.List (sort, nub)
import System.Environment (getArgs)
import Options.Applicative
import System.TimeIt (timeItT)
import Text.Printf

import Parsers.XMLParser (parseXML)
import OpenGL.Window
import OpenGL.Shader
import VectorGraphic
import Curve

------------------------------------------------------------

data Args = Args
  { vectorGraphicFile  :: FilePath
  , debug              :: Bool
  , vertexShaderFile   :: FilePath
  , geometryShaderFile :: FilePath
  , fragmentShaderFile :: FilePath
  }
  deriving (Show)

args :: Parser Args
args = Args
  <$> strArgument (value "" <> metavar "Vector graphic file")
  <*> switch (short 'd' <> long "debug" <> help "Enable debug")
  <*> strOption (short 'v' <> long "vertex" <> help "Vertex shader file" <> value "shaders/vertex.glsl" <> metavar "FILE")
  <*> strOption (short 'g' <> long "geometry" <> help "Geometry shader file" <> value "" <> metavar "FILE")
  <*> strOption (short 'f' <> long "fragment" <> help "Fragment shader file" <> value "shaders/fragment.glsl" <> metavar "FILE")

------------------------------------------------------------

main :: IO ()
main = execParser extraArgs >>= processArgs
  where shortDesc = "Diffusion Curves in Haskell, Chaoya Li <chaoya@chaoya.info> (C) 2015"
        extraArgs = info (helper <*> args) (fullDesc <> header shortDesc)
        parseProfile act = do
          (t, a) <- timeItT act
          printf "Parsing time: %6.2fs\n" t >> return a
        processArgs (Args [] _ v g f) = runOpenGL (ShaderContainer v g f) Nothing
        processArgs (Args fp False v g f) = readFile fp >>= (parseProfile.parseXML) >>= runOpenGL (ShaderContainer v g f)
        processArgs (Args fp True _ _ _) = readFile fp >>= (parseProfile.parseXML) >>= debugVectorGraphic fp

debugVectorGraphic :: FilePath -> Maybe VectorGraphic -> IO ()
debugVectorGraphic _ Nothing  = putStrLn "Please provide a valid vector graphic file."
debugVectorGraphic filepath (Just vg) = do
  let curves = vgCurves vg
      width  = vgWidth  vg
      height = vgHeight vg
      seesee curve = (crGlobalLen curve
                     ,crLifeTime curve
                     ,length . crControlPoints $ curve
                     ,getCurveLeftColorGidRange curve
                     ,getCurveRightColorGidRange curve
                     ,getCurveBlurPointGidRange curve)
      (xMin, yMin, xMax, yMax) = getVGBoundingBox vg
      nTotalCPs = foldl (\n curve -> n + (length.crControlPoints $ curve)) 0 curves

  printf "File: %s\n" filepath
  -- print vg
  printf "Size: (%d, %d)\n" width height
  printf "Number of curves: %d\n" (length curves)
  printf "Number of total control points: %d\n" nTotalCPs
  printf "Bounding box: (%.2f, %.2f, %.2f, %.2f)\n" xMin yMin xMax yMax
  -- mapM_ print $ sort $ nub $ map seesee curves

runOpenGL :: ShaderContainer -> Maybe VectorGraphic -> IO ()
runOpenGL shaders vg = newWindow 800 600 "Vector Graphics" shaders vg >>= runWindow

