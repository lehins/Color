{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.CMYKSpec (spec) where

import Graphics.Color.Space.Common
import qualified Graphics.Color.Space.RGB.Derived.SRGB as Derived
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (CMYK cs) e) where
  arbitrary =
    ColorCMYK <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator <*>
    arbitraryElevator

spec :: Spec
spec =
  describe "CMYK" $ do
    colorModelSpec @(CMYK (Derived.SRGB D65 'NonLinear)) @Word "CMYK"
    colorSpaceCommonSpec @(CMYK (Derived.SRGB D65 'NonLinear)) @Double $ pure ()
    -- Arbitrary inverse CMYKtoSRGB is not true.
    prop "sRGBtoCMYK" $ \ (srgb :: Color (Derived.SRGB D65 'NonLinear) Double) ->
      toBaseSpace (fromBaseSpace srgb :: Color (CMYK (Derived.SRGB D65 'NonLinear)) Double)
      `epsilonEqColor` srgb
