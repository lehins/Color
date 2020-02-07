{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.AdobeRGBSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.Derived.AdobeRGB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (AdobeRGB (i :: k)) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "AdobeRGB" $ do
  colorModelSpec @(AdobeRGB 'D65) @Word "AdobeRGB"
  colorSpaceSpec @(AdobeRGB 'D65) @Float
