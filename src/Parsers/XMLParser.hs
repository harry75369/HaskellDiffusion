module Parsers.XMLParser
( parseXML
) where

------------------------------------------------------------

import qualified Text.Parsec            as P
import qualified Parsers.ParserElements as E
import           Control.Monad (forM)
import           VectorGraphic
import           Curve

------------------------------------------------------------

p_xml = do
  let p_color = do
        g <- E.lexstr "G=" >> E.quotes E.integer
        r <- E.lexstr "R=" >> E.quotes E.integer
        gid <- E.lexstr "globalID=" >> E.quotes E.integer
        b <- E.lexstr "B=" >> E.quotes E.integer
        return (fromIntegral r,fromIntegral g,fromIntegral b,fromIntegral gid)

  -- XML header
  E.lexstr "<!DOCTYPE CurveSetXML>"

  -- Meta graphics info
  (width, height, ncurves) <- E.angles $ do
    E.lexstr "curve_set"
    w <- E.lexstr "image_width=" >> E.quotes E.integer
    h <- E.lexstr "image_height=" >> E.quotes E.integer
    n <- E.lexstr "nb_curves=" >> E.quotes E.integer
    return (fromIntegral w,fromIntegral h,n)

  -- Curves
  curves <- forM [1..ncurves] $ \i -> do

    -- Meta curve info
    (n_cp, n_lc, n_rc, n_bp, global_len, life_time) <- E.angles $ do
      E.lexstr "curve"
      n_cp <- E.lexstr "nb_control_points=" >> E.quotes E.integer
      n_lc <- E.lexstr "nb_left_colors=" >> E.quotes E.integer
      gl <- P.option (-1) $ E.lexstr "global_len=" >> E.quotes E.integer
      n_rc <- E.lexstr "nb_right_colors=" >> E.quotes E.integer
      n_bp <- E.lexstr "nb_blur_points=" >> E.quotes E.integer
      lt   <- E.lexstr "lifetime=" >> E.quotes E.integer
      return (n_cp, n_lc, n_rc, n_bp, fromIntegral gl, fromIntegral lt)

    -- Control points
    E.lexstr "<control_points_set>"
    control_points <- forM [1..n_cp] $ \j ->
      P.between (E.lexstr "<control_point") (E.lexstr "/>") $ do
        x <- E.lexstr "x=" >> E.quotes E.float
        y <- E.lexstr "y=" >> E.quotes E.float
        return (x,y)
    E.lexstr "</control_points_set>"

    -- Left colors
    E.lexstr "<left_colors_set>"
    left_colors <- forM [1..n_lc] $ \j ->
      P.between (E.lexstr "<left_color") (E.lexstr "/>") p_color
    E.lexstr "</left_colors_set>"

    -- Right colors
    E.lexstr "<right_colors_set>"
    right_colors <- forM [1..n_rc] $ \j ->
      P.between (E.lexstr "<right_color") (E.lexstr "/>") p_color
    E.lexstr "</right_colors_set>"

    -- Blur points
    E.lexstr "<blur_points_set>"
    blur_points <- forM [1..n_bp] $ \j ->
      P.between (E.lexstr "<best_scale") (E.lexstr "/>") $ do
        v <- E.lexstr "value=" >> E.quotes E.integer
        gid <- E.lexstr "globalID=" >> E.quotes E.integer
        return (fromIntegral v,fromIntegral gid)
    E.lexstr "</blur_points_set>"

    E.lexstr "</curve>"
    return $ Curve global_len life_time control_points left_colors right_colors blur_points

  E.lexstr "</curve_set>"
  return $ VectorGraphic width height curves

parseXML :: String -> IO (Maybe VectorGraphic)
parseXML s = case P.parse p_xml "XML" s of
  Left err -> print err >> return Nothing
  Right vg -> return $ Just vg

