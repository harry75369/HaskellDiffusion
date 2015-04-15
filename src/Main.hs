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
  let curves = vg_curves vg
      lgids = map (\(_,_,_,gid)->gid) $ concat $ map cr_left_colors curves
      rgids = map (\(_,_,_,gid)->gid) $ concat $ map cr_right_colors curves
      bgids = map (\(_,gid)->gid) $ concat $ map cr_blur_points curves
      gids = lgids ++ rgids ++ bgids
      glens = map cr_global_len curves
  print $ (minimum glens, maximum glens, minimum gids, maximum gids)
