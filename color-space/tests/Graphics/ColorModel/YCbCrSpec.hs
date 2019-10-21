{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorModel.YCbCrSpec (spec) where

import Graphics.ColorModel.Common
import Graphics.ColorModel.YCbCr
import Graphics.ColorModel.RGBSpec ()

instance (Elevator e, Random e) => Arbitrary (Pixel YCbCr e) where
  arbitrary = PixelYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "YCbCr" $ do
    colorModelSpec @YCbCr @Word
    it "rgb2ycbcr . ycbcr2rgb" $
      property $ \rgb -> epsilonEqPixelTol 1e-5 rgb (ycbcr2rgb (rgb2ycbcr rgb))
