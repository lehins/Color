{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1931.RGBSpec (spec) where

import Graphics.Color.Space.CIE1931.RGB
import Graphics.Color.Space.Common

instance (Elevator e, Random e) => Arbitrary (Color (CIERGB l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = describe "RGB" $ do
  colorModelSpec @(CIERGB 'NonLinear) @Word "CIERGB 'NonLinear"
  colorSpaceSpec @(CIERGB 'NonLinear) @Float
  colorModelSpec @(CIERGB 'Linear) @Int "CIERGB 'Linear"
  colorSpaceSpec @(CIERGB 'Linear) @Double
