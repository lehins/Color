{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common

instance (Elevator e, Random e) => Arbitrary (Color (SRGB l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(SRGB 'NonLinear) @Word "SRGB 'NonLinear"
  colorSpaceLenientSpec @(SRGB 'NonLinear) @Float 0.001
  colorModelSpec @(SRGB 'Linear) @Word "SRGB 'Linear"
  colorSpaceLenientSpec @(SRGB 'Linear) @Double 0.001
