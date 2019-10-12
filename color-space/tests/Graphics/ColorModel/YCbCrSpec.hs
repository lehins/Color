{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.YCbCrSpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator, epsilonEqPixelTol)
import Graphics.ColorModel.YCbCr
import Graphics.ColorModel.RGBSpec ()
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel YCbCr e) where
  arbitrary = PixelYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "YCbCr" $ do
    it "rgb2ycbcr . ycbcr2rgb" $
      property $ \rgb -> epsilonEqPixelTol 1e-5 rgb (ycbcr2rgb (rgb2ycbcr rgb))
    xit "ycbcr2rgb . rgb2ycbcr" $
      property $ \ycbcr -> epsilonEqPixelTol 1e-5 ycbcr (rgb2ycbcr (ycbcr2rgb ycbcr))
