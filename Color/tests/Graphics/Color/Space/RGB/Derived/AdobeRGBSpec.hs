{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.AdobeRGBSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.Common
import qualified Graphics.Color.Space.RGB.Derived.AdobeRGB as D

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (D.AdobeRGB (i :: k) l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "AdobeRGB" $ do
  colorModelSpec @(D.AdobeRGB 'D65 'NonLinear) @Word "AdobeRGB CIE1931 'D65 'NonLinear"
  colorSpaceSpec @(D.AdobeRGB 'D65 'NonLinear) @Float
  colorModelSpec @(D.AdobeRGB 'D50 'Linear) @Int "AdobeRGB CIE1931 'D50 'Linear"
  colorSpaceSpec @(D.AdobeRGB 'D50 'Linear) @Double
