{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.HSVSpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator, epsilonEqPixel, epsilonEqPixelTol)
import Graphics.ColorModel.HSV
import Graphics.ColorModel.RGBSpec (rgbs)
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel HSV e) where
  arbitrary = PixelHSV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSV" $ do
    it "rgb2hsv . hsv2rgb" $ property $ \ rgb -> rgb `epsilonEqPixel` hsv2rgb (rgb2hsv rgb)
    it "hsv2rgb . rgb2hsv" $ property $ \ hsv -> hsv `epsilonEqPixel` rgb2hsv (hsv2rgb hsv)
    describe "samples" $ do
      let tol = 1e-3
      prop "rgb2hsv" $ once $ conjoin $ zipWith (epsilonEqPixelTol tol) (rgb2hsv <$> rgbs) hsvs
      prop "hsv2rgb" $ once $ conjoin $ zipWith (epsilonEqPixelTol tol) rgbs (hsv2rgb <$> hsvs)


hsvs :: [Pixel HSV Double]
hsvs =
  [ PixelH360SV (0/0) 0 1
  , PixelH360SV (0/0) 0 0.5
  , PixelH360SV (0/0) 0 0
  , PixelH360SV 0.0 1 1
  , PixelH360SV 60.0 1 0.75
  , PixelH360SV 120.0 1 0.5
  , PixelH360SV 180.0 0.5 1
  , PixelH360SV 240.0 0.5 1
  , PixelH360SV 300.0 0.667 0.75
  , PixelH360SV 61.8 0.779 0.643
  , PixelH360SV 251.1 0.887 0.918
  , PixelH360SV 134.9 0.828 0.675
  , PixelH360SV 49.5 0.944 0.941
  , PixelH360SV 283.7 0.792 0.897
  , PixelH360SV 14.3 0.661 0.931
  , PixelH360SV 56.9 0.467 0.998
  , PixelH360SV 162.4 0.875 0.795
  , PixelH360SV 248.3 0.75 0.597
  , PixelH360SV 240.5 0.316 0.721
  ]
