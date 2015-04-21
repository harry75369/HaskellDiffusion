module OpenGL.Utility
( displayGLInfo
, getGLString
, getGLInteger
, getGLGen
, OpenGL.Utility.sizeOf
) where

------------------------------------------------------------

import Graphics.Rendering.OpenGL.Raw
import Control.Monad (liftM)
import Foreign.Ptr (Ptr(..), castPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..), poke, peek)
import Foreign.Marshal.Array (allocaArray, peekArray)
import qualified Foreign.Storable as S
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint (($+$), (<+>))

------------------------------------------------------------

displayGLInfo :: IO ()
displayGLInfo = do
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

getGLString :: GLenum -> IO String
getGLString enum = liftM castPtr (glGetString enum) >>= peekCString

getGLInteger :: GLenum -> IO GLint
getGLInteger enum = alloca $ \ptr -> poke ptr 0 >> glGetIntegerv enum ptr >> peek ptr

getGLGen :: (GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> IO [GLuint]
getGLGen genFunc n = allocaArray (fromIntegral n) $ \ptr -> genFunc n ptr >> peekArray (fromIntegral n) ptr

sizeOf :: Storable a => [a] -> Int
sizeOf [] = 0
sizeOf list@(x:xs) = length list * S.sizeOf x

