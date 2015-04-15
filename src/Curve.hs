module Curve
( Curve(..)
) where

------------------------------------------------------------

------------------------------------------------------------

data Curve = Curve
  { cr_global_len     :: Int
  , cr_control_points :: [(Double, Double)]
  , cr_left_colors    :: [(Int, Int, Int, Int)]
  , cr_right_colors   :: [(Int, Int, Int, Int)]
  , cr_blur_points    :: [(Int, Int)]
  }
  deriving (Show)

