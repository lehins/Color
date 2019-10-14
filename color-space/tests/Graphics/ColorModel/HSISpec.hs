{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.HSISpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (izipWithM_, arbitraryElevator, epsilonEqPixel, epsilonPixelIxSpec)
import Graphics.ColorModel.HSI
import Graphics.ColorModel.RGBSpec (rgbs)
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel HSI e) where
  arbitrary = PixelHSI <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSI" $ do
    it "rgb2hsi . hsi2rgb" $ property $ \rgb -> rgb `epsilonEqPixel` hsi2rgb (rgb2hsi rgb)
    it "hsi2rgb . rgb2hsi" $ property $ \hsi -> hsi `epsilonEqPixel` rgb2hsi (hsi2rgb hsi)
    describe "samples" $ do
      let tol = 1e-3
      describe "rgb2hsi" $ izipWithM_ (epsilonPixelIxSpec tol) hsis (rgb2hsi <$> rgbs)
      describe "hsi2rgb" $ izipWithM_ (epsilonPixelIxSpec tol) rgbs (hsi2rgb <$> hsis)


hsis :: [Pixel HSI Double]
hsis =
  [ PixelH360SI 0 0 1
  , PixelH360SI 0 0 0.5
  , PixelH360SI 0 0 0
  , PixelH360SI 0.0 1 0.333
  , PixelH360SI 60.0 1 0.5
  , PixelH360SI 120.0 1 0.167
  , PixelH360SI 180.0 0.4 0.833
  , PixelH360SI 240.0 0.25 0.667
  , PixelH360SI 300.0 0.571 0.583
  , PixelH360SI 61.5 0.699 0.471
  , PixelH360SI 250.0 0.756 0.426
  , PixelH360SI 133.8 0.667 0.349
  , PixelH360SI 50.5 0.911 0.593
  , PixelH360SI 284.8 0.686 0.596
  , PixelH360SI 13.2 0.446 0.57
  , PixelH360SI 57.4 0.363 0.835
  , PixelH360SI 163.4 0.8 0.495
  , PixelH360SI 247.3 0.533 0.319
  , PixelH360SI 240.4 0.135 0.57
  ]
