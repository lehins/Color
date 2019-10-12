{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.HSISpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator, epsilonEqPixel)
import Graphics.ColorModel.HSI
import Graphics.ColorModel.RGBSpec ()
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel HSI e) where
  arbitrary = PixelHSI <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSI" $ do
    it "rgb2hsi . hsi2rgb" $ property $ \ rgb -> rgb `epsilonEqPixel` hsi2rgb (rgb2hsi rgb)
    it "hsi2rgb . rgb2hsi" $ property $ \ hsi -> hsi `epsilonEqPixel` rgb2hsi (hsi2rgb hsi)
