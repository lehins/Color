{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.YUV.YCbCrSpec (spec) where

import Graphics.Color.Space.Common
-- import Graphics.Color.Space.YCbCr
-- import Graphics.Color.Space.RGBSpec ()

-- instance (Elevator e, Random e) => Arbitrary (Color YCbCr e) where
--   arbitrary = ColorYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = pure ()
  -- describe "YCbCr" $ do
  --   colorModelSpec @YCbCr @Word
  --   it "rgb2ycbcr . ycbcr2rgb" $
  --     property $ \rgb -> epsilonEqColorTol (1e-5 :: Double) rgb (ycbcr2rgb (rgb2ycbcr rgb))
