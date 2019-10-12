{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModel.RGBSpec (spec) where

import Graphics.ColorModel
import Graphics.ColorModelSpec (arbitraryElevator)
import Graphics.ColorModel.RGB
import System.Random
import Test.Hspec
import Test.QuickCheck

instance (Elevator e, Random e) => Arbitrary (Pixel RGB e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = pure ()
