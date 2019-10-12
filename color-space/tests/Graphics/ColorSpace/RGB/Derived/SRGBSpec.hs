{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.Derived.SRGBSpec (spec) where

import Graphics.ColorSpaceSpec hiding (spec)
import Graphics.ColorSpace.RGB.Derived.SRGB
import Graphics.ColorSpace.CIE1931.Illuminants
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel (RGB (i :: k)) e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  it "toFromPixelXYZ" $ property (prop_toFromPixelXYZ @(RGB 'D65) @Double)
  it "toFromColorSpace" $ property (prop_toFromColorSpace @(RGB 'D65) @Double)
