module OpenGL.Camera
( Camera(..)
, defaultCamera
, scaleCamera
, transCamera
, getMVPMatrix
) where

------------------------------------------------------------

--import Graphics.Rendering.OpenGL.Raw
import Graphics.GL

import Data.IORef
import Data.StateVar
import Linear

------------------------------------------------------------

data Camera = Camera
  { getScale :: IORef GLfloat
  , getTrans :: IORef (GLfloat, GLfloat)
  }

------------------------------------------------------------

defaultCamera :: IO Camera
defaultCamera = do
  scale <- newIORef 1.0
  trans <- newIORef (0.0, 0.0)
  return $ Camera scale trans

scaleCamera :: Camera -> GLfloat -> IO ()
scaleCamera camera ds = getScale camera $~ (*ds)

transCamera :: Camera -> (GLfloat, GLfloat) -> IO ()
transCamera camera (dx, dy) = getTrans camera $~ \(x, y) -> (x+dx, y-dy)

getMVPMatrix :: Camera -> IO (M44 GLfloat)
getMVPMatrix camera = do
  s <- get $ getScale camera
  (x, y) <- get $ getTrans camera
  return $ V4 (V4 s 0 0 x)
         (V4 0 s 0 y)
         (V4 0 0 s 0)
         (V4 0 0 0 1)
