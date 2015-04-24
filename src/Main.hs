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
        processArgs (Args fp True _ _ _) = readFile fp >>= (parseProfile.parseXML) >>= debugVectorGraphic

debugVectorGraphic :: Maybe VectorGraphic -> IO ()
debugVectorGraphic Nothing  = putStrLn "Please provide a valid vector graphic file."
debugVectorGraphic (Just vg) = do
  let curves = vgCurves vg
  let nControlPoints = length . crControlPoints
      gidRangeOfLeftColors curve =
        let colors = crLeftColors curve
            gids = map (\(_,_,_,gid)->gid) colors
         in (minimum gids, maximum gids)
      gidRangeOfRightColors curve =
        let colors = crRightColors curve
            gids = map (\(_,_,_,gid)->gid) colors
         in (minimum gids, maximum gids)
      gidRangeOfBlurPoints curve =
        let points = crBlurPoints curve
            gids = map snd points
         in (minimum gids, maximum gids)
      seesee curve = (nControlPoints curve, crGlobalLen curve, gidRangeOfLeftColors curve, gidRangeOfRightColors curve, gidRangeOfBlurPoints curve, crLifeTime curve)

  -- print vg
  mapM_ print $ sort $ nub $ map seesee curves

runOpenGL :: ShaderContainer -> Maybe VectorGraphic -> IO ()
runOpenGL shaders vg = newWindow 800 600 "Vector Graphics" shaders vg >>= runWindow
