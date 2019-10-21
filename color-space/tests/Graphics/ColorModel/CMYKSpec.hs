{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.CMYKSpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator, epsilonEqPixel)
import Graphics.ColorModel.CMYK
import Graphics.ColorModel.RGBSpec ()
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel CMYK e) where
  arbitrary =
    PixelCMYK <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator <*>
    arbitraryElevator

spec :: Spec
spec =
  describe "CMYK" $
    it "rgb2cmyk . cmyk2rgb" $ property $ \rgb -> rgb `epsilonEqPixel` cmyk2rgb (rgb2cmyk rgb)
