{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.CIERGBSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.RGB.Derived.CIERGB
import Graphics.Color.Space.Common

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (CIERGB (i :: k) l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = describe "RGB" $ do
  colorModelSpec @(CIERGB 'E 'NonLinear) @Word "CIERGB CIE1931 'E 'NonLinear"
  colorSpaceSpec @(CIERGB 'E 'NonLinear) @Float
  colorModelSpec @(CIERGB 'D75 'Linear) @Int "CIERGB CIE1931 'D75 'Linear"
  colorSpaceSpec @(CIERGB 'D75 'Linear) @Double
