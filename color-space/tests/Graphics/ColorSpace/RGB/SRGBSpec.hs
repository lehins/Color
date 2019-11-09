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
  prop "toFromPixelXYZ (lenient)" $ prop_toFromLenientPixelXYZ @SRGB @Double 0.001
  prop "toFromColorSpace" $ prop_toFromColorSpace @SRGB @Double
