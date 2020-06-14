{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.HSVSpec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB
import qualified Graphics.Color.Space.RGB.Derived.SRGB as Derived
import Graphics.Color.Space.RGB.Alternative.HSV
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (HSV cs) e) where
  arbitrary = ColorHSV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSV" $ do
    describe "Derived-sRGB" $ do
      colorModelSpec @(HSV (Derived.SRGB D65 'NonLinear)) @Word "HSV"
      colorSpaceSpec @(HSV (Derived.SRGB D65 'NonLinear)) @Double
      colorModelSpec @(HSV (Derived.SRGB D65 'Linear)) @Int "HSV"
      colorSpaceSpec @(HSV (Derived.SRGB D65 'Linear)) @Float
