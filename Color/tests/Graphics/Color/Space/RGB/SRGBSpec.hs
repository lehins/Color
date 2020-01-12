{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB

instance (Elevator e, Random e) => Arbitrary (Color SRGB e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @SRGB @Word "SRGB"
  colorSpaceLenientSpec @SRGB @_ @Float 0.001
