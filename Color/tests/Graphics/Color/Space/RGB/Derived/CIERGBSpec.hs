{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.CIERGBSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import qualified Graphics.Color.Space.RGB.Derived.CIERGB as D
import Graphics.Color.Space.Common

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (D.CIERGB (i :: k) l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = describe "RGB" $ do
  colorModelSpec @(D.CIERGB 'E 'NonLinear) @Word "CIERGB CIE1931 'E 'NonLinear"
  colorSpaceSpec @(D.CIERGB 'E 'NonLinear) @Float
  colorModelSpec @(D.CIERGB 'D75 'Linear) @Int "CIERGB CIE1931 'D75 'Linear"
  colorSpaceSpec @(D.CIERGB 'D75 'Linear) @Double
