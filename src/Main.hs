module Main
( main
) where

------------------------------------------------------------

import Control.Monad (mapM_)
import Data.List (sort, nub)
import System.Environment (getArgs)

import Parsers.XMLParser (parseXML)
import OpenGL.Window
import VectorGraphic
import Curve

------------------------------------------------------------

main :: IO ()
main = do
  Just vg <- fmap head getArgs >>= readFile >>= parseXML
  -- print vg
  -- debugVectorGraphics vg
  debugOpenGL

debugVectorGraphics vg = do
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
  mapM_ print $ sort $ nub $ map seesee curves

debugOpenGL = do
  win <- makeWindow 800 600 "Vector Graphics"
  displayGLInfo win
  runWindow win
