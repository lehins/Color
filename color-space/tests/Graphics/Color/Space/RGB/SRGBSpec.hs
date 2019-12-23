{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB

instance (Elevator e, Random e) => Arbitrary (Pixel SRGB e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @SRGB @Word
  prop "toFromPixelXYZ (lenient)" $ prop_toFromLenientPixelXYZ @SRGB @_ @Double 0.001
  prop "toFromColorSpace" $ prop_toFromColorSpace @SRGB @_ @Double
