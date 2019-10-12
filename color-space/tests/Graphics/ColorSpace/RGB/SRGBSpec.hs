{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.SRGBSpec (spec, arbitraryElevator) where

import Graphics.ColorSpaceSpec hiding (spec)
import Graphics.ColorSpace.RGB.SRGB
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel SRGB e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  it "toFromLenientPixelXYZ" $ property (prop_toFromLenientPixelXYZ @SRGB @Double 0.001)
  it "toFromColorSpace" $ property (prop_toFromColorSpace @SRGB @Double)
