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
  let processArgsWith f []    = f Nothing
      processArgsWith f (x:_) = readFile x >>= parseXML >>= f

  getArgs >>=
    -- processArgsWith debugVectorGraphic
    processArgsWith runOpenGL

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

runOpenGL :: Maybe VectorGraphic -> IO ()
runOpenGL vg = newWindow 800 600 "Vector Graphics" vg >>= runWindow
