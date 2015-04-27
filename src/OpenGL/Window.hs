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
import           System.FilePath (takeFileName)
import           Data.Bits ((.|.))
import           Foreign.Marshal.Utils (with)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr (Ptr(..), nullPtr, plusPtr, castPtr)
import           Foreign.C.String (withCString)
import           Data.StateVar
import           Data.IORef

import OpenGL.Utility
import OpenGL.Shader
import OpenGL.Camera
import VectorGraphic

------------------------------------------------------------

data UniformLocations = DefaultLocations
  { getMVPMatrixLoc  :: GLint
  }
  | FlamesLocations
  { getResolutionLoc :: GLint
  , getGlobalTimeLoc :: GLint
  }
  deriving (Show)

data OpenGLStates = OpenGLStates
  { getVao :: GLuint
  , getVbo :: GLuint
  , getPid :: GLuint
  , getNVertices  :: GLsizei
  , getULocations :: UniformLocations
  , getGlobalTime :: IORef GLfloat
  }

data WindowContainer = WindowContainer
  { getWindow :: GLFW.Window
  , getStates :: OpenGLStates
  , getCamera :: Camera
  , getLastXY :: IORef (Double, Double)
  , getVecimg :: Maybe VectorGraphic
  }

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
  camera <- defaultCamera
  lastXY <- newIORef (-1, -1)
  let windowContainer = WindowContainer win states camera lastXY vg

  -- Setup callbacks
  GLFW.setWindowCloseCallback   win (Just $ closeWindow windowContainer)
  GLFW.setWindowRefreshCallback win (Just $ drawWindow windowContainer)
  GLFW.setWindowSizeCallback    win (Just $ resizeWindow windowContainer)
  GLFW.setKeyCallback           win (Just $ keyCallback windowContainer)
  GLFW.setMouseButtonCallback   win (Just $ mouseButtonCallback windowContainer)
  GLFW.setCursorPosCallback     win (Just $ mousePosCallback windowContainer)
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
    win        = getWindow windowContainer
    nVertices  = getNVertices  . getStates $ windowContainer
    uLocations = getULocations . getStates $ windowContainer
    globalTime = getGlobalTime . getStates $ windowContainer
    camera     = getCamera windowContainer

    updateUniforms (FlamesLocations resolutionLoc globalTimeLoc) = do
      (w, h) <- GLFW.getFramebufferSize win
      t <- get globalTime
      glUniform1f globalTimeLoc t
      glUniform2f resolutionLoc (fromIntegral w) (fromIntegral h)
      globalTime $~ (+0.1)

    updateUniforms (DefaultLocations mvpMatrixLoc) = do
      m <- getMVPMatrix camera
      with m $ \ptr -> glUniformMatrix4fv mvpMatrixLoc 1 (fromIntegral gl_TRUE) (castPtr ptr)

    -- type WindowRefreshCallback = Window -> IO ()
    callback :: GLFW.WindowRefreshCallback
    callback win = do
      glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
      updateUniforms uLocations >> glDrawArrays gl_TRIANGLE_STRIP 0 nVertices

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

mouseButtonCallback windowContainer = callback
  where
    lastXY = getLastXY windowContainer

    -- type MouseButtonCallback = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
    {- From GLFW source code:
    #define GLFW_MOUSE_BUTTON_LAST   GLFW_MOUSE_BUTTON_8
    #define GLFW_MOUSE_BUTTON_LEFT   GLFW_MOUSE_BUTTON_1
    #define GLFW_MOUSE_BUTTON_RIGHT  GLFW_MOUSE_BUTTON_2
    #define GLFW_MOUSE_BUTTON_MIDDLE GLFW_MOUSE_BUTTON_3
    -}
    callback :: GLFW.MouseButtonCallback
    callback win GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed  _ = GLFW.getCursorPos win >>= ($=) lastXY
    callback win GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = lastXY $= (-1, -1)

mousePosCallback windowContainer = callback
  where
    camera = getCamera windowContainer
    lastXY = getLastXY windowContainer
    cvt = fromRational . toRational

    -- type CursorPosCallback = Window -> Double -> Double -> IO ()
    callback :: GLFW.CursorPosCallback
    callback win x y = do
      (w, h) <- GLFW.getFramebufferSize win
      xy <- get lastXY
      case xy of
        (-1, -1) -> return ()
        (lastX, lastY) -> do
          transCamera camera (cvt $ (x - lastX)/(fromIntegral w), cvt $ (y - lastY)/(fromIntegral h))
          lastXY $= (x, y)

scrollCallback windowContainer = callback
  where
    camera = getCamera windowContainer

    -- type ScrollCallback = Window -> Double -> Double -> IO ()
    callback :: GLFW.ScrollCallback
    callback win x y = scaleCamera camera $ if y > 0 then 1.25 else 0.8

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

  -- Setup uniforms
  let setupUniforms "flames.frag" = do
        resolutionLoc <- withCString "iResolution" $ \ptr -> glGetUniformLocation pid ptr
        globalTimeLoc <- withCString "iGlobalTime" $ \ptr -> glGetUniformLocation pid ptr
        return $ FlamesLocations resolutionLoc globalTimeLoc
      setupUniforms _             = do
        mvpMatrixLoc <- withCString "iMVPMatrix" $ \ptr -> glGetUniformLocation pid ptr
        return $ DefaultLocations mvpMatrixLoc
  uLocations <- setupUniforms $ takeFileName $ fragmentShaderPath shaders

  -- Init global time
  globalTime <- newIORef 0

  return $ OpenGLStates vao vbo pid nVertices uLocations globalTime

