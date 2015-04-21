module OpenGL.Window
( newWindow
, runWindow
) where

------------------------------------------------------------

import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW
import qualified Codec.Picture as J

import           Control.Monad (forever, liftM)
import           System.Exit (exitSuccess)
import           Data.Bits ((.|.))
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr (Ptr(..), nullPtr, plusPtr)
import           Foreign.C.String (withCString)

import OpenGL.Utility
import OpenGL.Shader
import OpenGL.Camera
import VectorGraphic

------------------------------------------------------------

data OpenGLStates = OpenGLStates
  { getVao :: GLuint
  , getVbo :: GLuint
  , getPid :: GLuint
  , getNVertices :: GLsizei
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

newWindow :: Int -> Int -> String -> ShaderContainer -> Maybe VectorGraphic -> IO WindowContainer
newWindow width height title shaders vg = do
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
  GLFW.makeContextCurrent (Just win) >> displayGLInfo
  states <- initializeGL shaders
  let camera = defaultCamera
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
    drawWindow windowContainer win
    GLFW.swapBuffers win

------------------------------------------------------------

closeWindow windowContainer = callback
  where
    -- type WindowCloseCallback = Window -> IO ()
    callback :: GLFW.WindowCloseCallback
    callback win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess

drawWindow windowContainer = callback
  where
    nVertices = getNVertices $ getStates windowContainer

    -- type WindowRefreshCallback = Window -> IO ()
    callback :: GLFW.WindowRefreshCallback
    callback win = do
      glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
      glDrawArrays gl_TRIANGLE_STRIP 0 nVertices

resizeWindow windowContainer = callback
  where
    -- type WindowSizeCallback = Window -> Int -> Int -> IO ()
    callback :: GLFW.WindowSizeCallback
    callback win _ _ = do
      (w, h) <- GLFW.getFramebufferSize win
      glViewport 0 0 (fromIntegral w) (fromIntegral h) >> glFlush

keyCallback windowContainer = callback
  where
    -- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    callback :: GLFW.KeyCallback
    callback win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow windowContainer win
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

------------------------------------------------------------

initializeGL :: ShaderContainer -> IO OpenGLStates
initializeGL shaders = do
  glEnable gl_DEPTH_TEST
  glClearColor 0.5 0.5 0.5 1.0

  -- Generate and bind vertex array object
  [vao] <- getGLGen glGenVertexArrays 1
  glBindVertexArray vao

  -- Generate and bind vertex buffer object
  [vbo] <- getGLGen glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo

  -- Initialize screen quad
  let nVertices = 4 :: GLsizei
      vertexPos = [-1, -1, -1, 1, 1, -1, 1, 1] :: [GLfloat]
      texCoords = [0, 0, 0, 1, 1, 0, 1, 1] :: [GLfloat]
      sizeVertexPos = fromIntegral $ sizeOf vertexPos
      sizeTexCoords = fromIntegral $ sizeOf texCoords
  glBufferData gl_ARRAY_BUFFER (sizeVertexPos + sizeTexCoords) nullPtr gl_STATIC_DRAW
  withArray vertexPos $ \ptr -> glBufferSubData gl_ARRAY_BUFFER 0 sizeVertexPos ptr
  withArray texCoords $ \ptr -> glBufferSubData gl_ARRAY_BUFFER sizeVertexPos sizeTexCoords ptr

  -- Load shaders
  pid <- loadShaders shaders
  glUseProgram pid
  vertexPosLoc <- liftM fromIntegral (withCString "vertexPos" $ \ptr -> glGetAttribLocation pid ptr)
  texCoordsLoc <- liftM fromIntegral (withCString "texCoords" $ \ptr -> glGetAttribLocation pid ptr)

  -- Setup vertex attributes
  glVertexAttribPointer vertexPosLoc 2 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
  glVertexAttribPointer texCoordsLoc 2 gl_FLOAT (fromIntegral gl_FALSE) 0 (plusPtr nullPtr $ fromIntegral sizeVertexPos)
  glEnableVertexAttribArray vertexPosLoc
  glEnableVertexAttribArray texCoordsLoc

  return $ OpenGLStates vao vbo pid nVertices

