{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.YUV.YCbCrSpec (spec) where

import Graphics.ColorSpace.Common
-- import Graphics.ColorSpace.YCbCr
-- import Graphics.ColorSpace.RGBSpec ()

-- instance (Elevator e, Random e) => Arbitrary (Pixel YCbCr e) where
--   arbitrary = PixelYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = pure ()
  -- describe "YCbCr" $ do
  --   colorModelSpec @YCbCr @Word
  --   it "rgb2ycbcr . ycbcr2rgb" $
  --     property $ \rgb -> epsilonEqPixelTol (1e-5 :: Double) rgb (ycbcr2rgb (rgb2ycbcr rgb))
