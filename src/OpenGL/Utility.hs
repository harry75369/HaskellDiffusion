module OpenGL.Utility
( getGLString
, getGLInteger
, getGLGen
) where

------------------------------------------------------------

import Graphics.Rendering.OpenGL.Raw
import Control.Monad (liftM)
import Foreign.Ptr (Ptr(..), castPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Array (allocaArray, peekArray)

------------------------------------------------------------

getGLString :: GLenum -> IO String
getGLString enum = liftM castPtr (glGetString enum) >>= peekCString

getGLInteger :: GLenum -> IO GLint
getGLInteger enum = alloca $ \ptr -> poke ptr 0 >> glGetIntegerv enum ptr >> peek ptr

getGLGen :: (GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> IO [GLuint]
getGLGen genFunc n = allocaArray (fromIntegral n) $ \ptr -> genFunc n ptr >> peekArray (fromIntegral n) ptr
