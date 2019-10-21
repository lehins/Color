{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.RGB.SRGB

instance (Elevator e, Random e) => Arbitrary (Pixel SRGB e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @SRGB @Word
  it "toFromLenientPixelXYZ" $ property (prop_toFromLenientPixelXYZ @SRGB @Double 0.001)
  it "toFromColorSpace" $ property (prop_toFromColorSpace @SRGB @Double)
