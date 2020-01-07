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

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (SRGB (i :: k)) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(SRGB 'D65) @Word "SRGB"
  prop "toFromColorXYZ" (prop_toFromColorXYZ :: Color (SRGB 'D65) Double -> Property)
  prop "toFromColorSpace" (prop_toFromColorSpace :: Color (SRGB 'D65) Double -> Property)
