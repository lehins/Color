{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.HSISpec (spec) where

import Graphics.Color.Space.Common
import qualified Graphics.Color.Space.RGB.Derived.SRGB as D
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (HSI cs) e) where
  arbitrary = ColorHSI <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSI" $ do
    describe "Derived-sRGB" $ do
      colorModelSpec @(HSI (D.SRGB D65 'NonLinear)) @Word "HSI"
      colorSpaceSpec @(HSI (D.SRGB D65 'NonLinear)) @Double
