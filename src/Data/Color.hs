module Data.Color
( Color(..)
) where

------------------------------------------------------------

------------------------------------------------------------

data Color a = Color
  { redChannel   :: a
  , greenChannel :: a
  , blueChannel  :: a
  }
  deriving (Show)

instance Functor Color where
  fmap f (Color r g b) = Color (f r) (f g) (f b)

instance (RealFloat a) => Num (Color a) where
  {-# SPECIALISE instance Num (Color Float) #-}
  {-# SPECIALISE instance Num (Color Double) #-}
  (Color r0 g0 b0) + (Color r1 g1 b1) = Color (r0+r1) (g0+g1) (b0+b1)
  (Color r0 g0 b0) - (Color r1 g1 b1) = Color (r0-r1) (g0-g1) (b0-b1)
  (Color r0 g0 b0) * (Color r1 g1 b1) = Color (r0*r1) (g0*g1) (b0*b1)
  negate color  = fmap negate color
  abs    color  = fmap abs color
  signum color  = fmap signum color
  fromInteger n = fmap fromInteger $ Color n n n

instance (RealFloat a) => Fractional (Color a) where
  {-# SPECIALISE instance Fractional (Color Float) #-}
  {-# SPECIALISE instance Fractional (Color Double) #-}
  (Color r0 g0 b0) / (Color r1 g1 b1) = Color (r0/r1) (g0/g1) (b0/b1)
  recip color = fmap recip color
  fromRational a = fmap fromRational $ Color a a a

------------------------------------------------------------

