{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB

instance (Elevator e, Random e) => Arbitrary (Color SRGB e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @SRGB @Word
  prop "toFromColorXYZ (lenient)" $ prop_toFromLenientColorXYZ @SRGB @_ @Double 0.001
  prop "toFromColorSpace" $ prop_toFromColorSpace @SRGB @_ @Double
