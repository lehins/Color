{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.HSVSpec (spec) where

import Graphics.Color.Space.Common
import qualified Graphics.Color.Space.RGB.Derived.SRGB as D
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (HSV cs) e) where
  arbitrary = ColorHSV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSV" $ do
    describe "Derived-sRGB" $ do
      colorModelSpec @(HSV (D.SRGB D65 'NonLinear)) @Word "HSV"
      colorSpaceSpec @(HSV (D.SRGB D65 'NonLinear)) @Double
      colorModelSpec @(HSV (D.SRGB D65 'Linear)) @Int "HSV"
      colorSpaceSpec @(HSV (D.SRGB D65 'Linear)) @Float
