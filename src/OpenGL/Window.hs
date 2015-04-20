module OpenGL.Window
( newWindow
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
import           Data.Bits ((.|.))

import OpenGL.Utility (getGLString, getGLInteger)
import OpenGL.Camera
import VectorGraphic

------------------------------------------------------------

data OpenGLStates = OpenGLStates
  { getVao :: GLuint
  , getVbo :: GLuint
  }
  deriving (Show)

data WindowContainer = WindowContainer
  { getWindow :: GLFW.Window
  , getStates :: OpenGLStates
  , getCamera :: Camera
  , getVecimg :: Maybe VectorGraphic
  }
  deriving (Show)

------------------------------------------------------------

newWindow :: Int -> Int -> String -> Maybe VectorGraphic -> IO WindowContainer
newWindow width height title vg = do
  True <- GLFW.init

  -- Setting window hints by first resetting to defaults
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  -- Create window and initilize OpenGL
  Just win <- GLFW.createWindow width height title Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  let states = OpenGLStates 0 0
  let camera = defaultCamera
  glEnable gl_DEPTH_TEST
  glClearColor 0.5 0.5 0.5 1.0
  let windowContainer = WindowContainer win states camera vg

  -- Setup callbacks
  GLFW.setWindowCloseCallback   win (Just $ closeWindow windowContainer)
  GLFW.setWindowRefreshCallback win (Just $ drawWindow windowContainer)
  GLFW.setWindowSizeCallback    win (Just $ resizeWindow windowContainer)
  GLFW.setKeyCallback           win (Just $ keyCallback windowContainer)
  GLFW.setMouseButtonCallback   win (Just $ mouseCallback windowContainer)
  GLFW.setScrollCallback        win (Just $ scrollCallback windowContainer)

  -- Return window
  return windowContainer

runWindow :: WindowContainer -> IO ()
runWindow windowContainer = do
  let win = getWindow windowContainer
  forever $ do
    GLFW.pollEvents
    drawWindow windowContainer $ win
    GLFW.swapBuffers win

displayGLInfo :: WindowContainer -> IO WindowContainer
displayGLInfo windowContainer = do
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

  return windowContainer

------------------------------------------------------------

closeWindow windowContainer = callback
  where
    -- type WindowCloseCallback = Window -> IO ()
    callback :: GLFW.WindowCloseCallback
    callback win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess

drawWindow windowContainer = callback
  where
    -- type WindowRefreshCallback = Window -> IO ()
    callback :: GLFW.WindowRefreshCallback
    callback win = do
      glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

resizeWindow windowContainer = callback
  where
    -- type WindowSizeCallback = Window -> Int -> Int -> IO ()
    callback :: GLFW.WindowSizeCallback
    callback win _ _ = do
      (w, h) <- GLFW.getFramebufferSize win
      glViewport 0 0 (fromIntegral w) (fromIntegral h)

keyCallback windowContainer = callback
  where
    -- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    callback :: GLFW.KeyCallback
    callback win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow windowContainer $ win
    callback _   _               _ _                     _ = return ()

mouseCallback windowContainer = callback
  where
    -- type MouseButtonCallback = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
    callback :: GLFW.MouseButtonCallback
    callback win _ _ _ = return ()

scrollCallback windowContainer = callback
  where
    -- type ScrollCallback = Window -> Double -> Double -> IO ()
    callback :: GLFW.ScrollCallback
    callback win _ _ = return ()
