{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.HSLSpec (spec) where

import Graphics.Color.Space.Common
import qualified Graphics.Color.Space.RGB.Derived.SRGB as D
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (HSL cs) e) where
  arbitrary = ColorHSL <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSL" $ do
    describe "Derived-sRGB" $ do
      colorModelSpec @(HSL (D.SRGB D65 'NonLinear)) @Word "HSL"
      colorSpaceSpec @(HSL (D.SRGB D65 'NonLinear)) @Double
