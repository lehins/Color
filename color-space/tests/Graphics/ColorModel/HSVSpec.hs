{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.HSVSpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator, epsilonEqPixel)
import Graphics.ColorModel.HSV
import Graphics.ColorModel.RGBSpec ()
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel HSV e) where
  arbitrary = PixelHSV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSV" $ do
    it "rgb2hsv . hsv2rgb" $ property $ \ rgb -> rgb `epsilonEqPixel` hsv2rgb (rgb2hsv rgb)
    it "hsv2rgb . rgb2hsv" $ property $ \ hsv -> hsv `epsilonEqPixel` rgb2hsv (hsv2rgb hsv)
