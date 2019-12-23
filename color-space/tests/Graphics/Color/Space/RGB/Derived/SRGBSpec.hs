{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.SRGBSpec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.RGB.Derived.SRGB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Pixel (SRGB (i :: k)) e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(SRGB 'D65) @Word
  prop "toFromPixelXYZ" (prop_toFromPixelXYZ :: Pixel (SRGB 'D65) Double -> Property)
  prop "toFromColorSpace" (prop_toFromColorSpace :: Pixel (SRGB 'D65) Double -> Property)
