{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.AdobeRGBSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.Derived.AdobeRGB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (AdobeRGB (i :: k) l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "AdobeRGB" $ do
  colorModelSpec @(AdobeRGB 'D65 'NonLinear) @Word "AdobeRGB CIE1931 'D65 'NonLinear"
  colorSpaceSpec @(AdobeRGB 'D65 'NonLinear) @Float
  colorModelSpec @(AdobeRGB 'D50 'Linear) @Int "AdobeRGB CIE1931 'D50 'Linear"
  colorSpaceSpec @(AdobeRGB 'D50 'Linear) @Double
