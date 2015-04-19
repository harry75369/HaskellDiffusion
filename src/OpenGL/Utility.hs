module OpenGL.Utility
( getGLString
, getGLInteger
) where

------------------------------------------------------------

import Graphics.Rendering.OpenGL.Raw (GLint, GLenum, glGetString, glGetIntegerv)
import Control.Monad (liftM)
import Foreign.Ptr (castPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke, peek)

------------------------------------------------------------

getGLString :: GLenum -> IO String
getGLString enum = liftM castPtr (glGetString enum) >>= peekCString

getGLInteger :: GLenum -> IO GLint
getGLInteger enum = alloca $ \ptr -> poke ptr 0 >> glGetIntegerv enum ptr >> peek ptr
