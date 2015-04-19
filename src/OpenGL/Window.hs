module OpenGL.Window
( makeWindow
, closeWindow
, runWindow
, displayGLInfo
) where

------------------------------------------------------------

import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW

import           Control.Monad (forever)
import           System.Exit (exitSuccess)
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint (($+$), (<+>))

import OpenGL.Utility (getGLString, getGLInteger)

------------------------------------------------------------

makeWindow :: Int -> Int -> String -> IO GLFW.Window
makeWindow width height title = do
  True <- GLFW.init

  -- Setting window hints by first resetting to defaults
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  -- Create window
  Just win <- GLFW.createWindow width height title Nothing Nothing
  GLFW.makeContextCurrent (Just win)

  -- Setup callbacks
  let keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow win
      keyPressed _   _               _ _                     _ = return ()
  GLFW.setWindowCloseCallback win (Just closeWindow)
  GLFW.setKeyCallback win (Just keyPressed)

  -- Return window
  return win

closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

runWindow :: GLFW.Window -> IO ()
runWindow win =
  forever $ do
    GLFW.pollEvents
    GLFW.swapBuffers win

displayGLInfo :: GLFW.Window -> IO ()
displayGLInfo win = do
  vendor   <- getGLString gl_VENDOR
  version  <- getGLString gl_VERSION
  renderer <- getGLString gl_RENDERER
  samples      <- getGLInteger gl_SAMPLES
  texture_size <- getGLInteger gl_MAX_TEXTURE_SIZE

  putStrLn $ Pretty.render $ Pretty.nest 0 (
        Pretty.text "Vendor:"       <+> Pretty.text vendor
    $+$ Pretty.text "Version:"      <+> Pretty.text version
    $+$ Pretty.text "Renderer:"     <+> Pretty.text renderer
    $+$ Pretty.text "Samples:"      <+> (Pretty.text . show) samples
    $+$ Pretty.text "Texture Size:" <+> (Pretty.text . show) texture_size
    )
