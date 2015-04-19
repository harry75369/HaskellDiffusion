module Main
( main
) where

------------------------------------------------------------

import System.Environment
import Parsers.XMLParser (parseXML)

import VectorGraphic
import Curve

import Control.Monad
import Data.List (sort, nub)

------------------------------------------------------------

main :: IO ()
main = do
  Just vg <- fmap head getArgs >>= readFile >>= parseXML
  -- print vg
  debugVectorGraphics vg

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
