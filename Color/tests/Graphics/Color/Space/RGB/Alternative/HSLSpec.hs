{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.HSLSpec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB
import qualified Graphics.Color.Space.RGB.Derived.SRGB as Derived
import Graphics.Color.Space.RGB.Alternative.HSL
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (HSL cs) e) where
  arbitrary = ColorHSL <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSL" $ do
    describe "Derived-sRGB" $ do
      colorModelSpec @(HSL (Derived.SRGB D65 'NonLinear)) @Word "HSL"
      colorSpaceSpec @(HSL (Derived.SRGB D65 'NonLinear)) @Double
