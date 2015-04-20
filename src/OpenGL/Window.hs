module OpenGL.Window
( Window(..)
, newWindow
, runWindow
, displayGLInfo
) where

------------------------------------------------------------

import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW
import qualified Codec.Picture as J

import           Control.Monad (forever)
import           System.Exit (exitSuccess)
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint (($+$), (<+>))

import OpenGL.Utility (getGLString, getGLInteger)
import OpenGL.Camera
import VectorGraphic

------------------------------------------------------------

data Window = Window
  { window :: GLFW.Window
  , camera :: Camera
  , vecimg :: Maybe VectorGraphic
  }
  deriving (Show)

------------------------------------------------------------

newWindow :: Int -> Int -> String -> Maybe VectorGraphic -> IO Window
newWindow width height title vg = do
  True <- GLFW.init

  -- Setting window hints by first resetting to defaults
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  -- Create window
  Just win <- GLFW.createWindow width height title Nothing Nothing
  GLFW.makeContextCurrent (Just win)

  -- Setup callbacks
  let closeWindow win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess
      keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow win
      keyPressed _   _               _ _                     _ = return ()
  GLFW.setWindowCloseCallback win (Just closeWindow)
  GLFW.setWindowRefreshCallback win Nothing
  GLFW.setWindowSizeCallback win Nothing
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setMouseButtonCallback win Nothing
  GLFW.setScrollCallback win Nothing

  -- Return window
  return $ Window win defaultCamera vg

runWindow :: Window -> IO ()
runWindow win =
  forever $ do
    GLFW.pollEvents
    GLFW.swapBuffers (window win)

displayGLInfo :: Window -> IO Window
displayGLInfo win = do
  vendor   <- getGLString gl_VENDOR
  version  <- getGLString gl_VERSION
  renderer <- getGLString gl_RENDERER
  samples      <- getGLInteger gl_SAMPLES
  texture_size <- getGLInteger gl_MAX_TEXTURE_SIZE

  putStrLn $ Pretty.render $ Pretty.nest 0 (
        Pretty.text "=================================================="
    $+$ Pretty.text "Vendor:      " <+> Pretty.text vendor
    $+$ Pretty.text "Version:     " <+> Pretty.text version
    $+$ Pretty.text "Renderer:    " <+> Pretty.text renderer
    $+$ Pretty.text "Samples:     " <+> (Pretty.text . show) samples
    $+$ Pretty.text "Texture Size:" <+> (Pretty.text . show) texture_size
    $+$ Pretty.text "=================================================="
    )

  return win

