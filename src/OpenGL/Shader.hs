module OpenGL.Shader
( ShaderContainer(..)
, loadShaders
) where

------------------------------------------------------------

import Graphics.Rendering.OpenGL.Raw
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (poke, peek)

------------------------------------------------------------

data ShaderContainer = ShaderContainer
  { vertexShaderPath :: FilePath
  , geometryShaderPath :: FilePath
  , fragmentShaderPath :: FilePath
  }

loadShaders :: ShaderContainer -> IO GLuint
loadShaders (ShaderContainer vp gp fp) = do
  let compileShader shaderType path = do
        putStrLn $ "Compiling shader: " ++ path

        -- Read source code from file
        sourcecode <- readFile path

        -- Create shader object and upload source code
        shaderID <- glCreateShader shaderType
        withCString sourcecode $ \ptr -> with ptr $ \pptr ->
          glShaderSource shaderID 1 pptr nullPtr

        -- Compile shader and check status
        glCompileShader shaderID >> checkShaderStatus shaderID

        return shaderID

      attachShader _ _ [] = return ()
      attachShader programID shaderType path = do
        shaderID <- compileShader shaderType path
        glAttachShader programID shaderID
        glDeleteShader shaderID

  -- Create program and attach shaders
  programID <- glCreateProgram
  attachShader programID gl_VERTEX_SHADER vp
  attachShader programID gl_GEOMETRY_SHADER gp
  attachShader programID gl_FRAGMENT_SHADER fp

  -- Link program and check status
  glLinkProgram programID
  checkProgramStatus programID

  return programID

checkShaderStatus shaderID = alloca $ \pstatus -> alloca $ \plen -> do
  poke pstatus 0 >> poke plen 0
  glGetShaderiv shaderID gl_COMPILE_STATUS pstatus
  glGetShaderiv shaderID gl_INFO_LOG_LENGTH plen
  len <- peek plen
  msg <- alloca $ \pmsg ->
    glGetShaderInfoLog shaderID len nullPtr pmsg >> peekCString pmsg
  putStr "Shader status: "
  putStrLn $ if null msg then "OK" else msg

checkProgramStatus programID = alloca $ \pstatus -> alloca $ \plen -> do
  poke pstatus 0 >> poke plen 0
  glGetProgramiv programID gl_LINK_STATUS pstatus
  glGetProgramiv programID gl_INFO_LOG_LENGTH plen
  len <- peek plen
  msg <- alloca $ \pmsg ->
    glGetProgramInfoLog programID len nullPtr pmsg >> peekCString pmsg
  putStr "Program status: "
  putStrLn $ if null msg then "OK" else msg

