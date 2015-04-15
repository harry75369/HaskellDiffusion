module Main
( main
) where

------------------------------------------------------------

import System.Environment
import Parsers.XMLParser (parseXML)

import VectorGraphic
import Curve

------------------------------------------------------------

main :: IO ()
main = fmap head getArgs >>= readFile >>= parseXML >>= print
--main = fmap head getArgs >>= readFile >>= parseXML >>= testGlobalLenAndID

testGlobalLenAndID vg = do
  let curves = vgCurves vg
      lgids = map (\(_,_,_,gid)->gid) $ concatMap crLeftColors curves
      rgids = map (\(_,_,_,gid)->gid) $ concatMap crRightColors curves
      bgids = map snd $ concatMap crBlurPoints curves
      gids = lgids ++ rgids ++ bgids
      glens = map crGlobalLen curves
  print (minimum glens, maximum glens, minimum gids, maximum gids)
