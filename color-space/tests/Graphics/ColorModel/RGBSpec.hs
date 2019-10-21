{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorModel.RGBSpec (spec, rgbs) where

import Graphics.ColorModel.RGB
import Graphics.ColorModel.Common

instance (Elevator e, Random e) => Arbitrary (Pixel RGB e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec =
  describe "RGB" $
    colorModelSpec @RGB @Word


rgbs :: [Pixel RGB Double]
rgbs =
  [ PixelRGB 1 1 1
  , PixelRGB 0.5 0.5 0.5
  , PixelRGB 0 0 0
  , PixelRGB 1 0 0
  , PixelRGB 0.75 0.75 0
  , PixelRGB 0 0.5 0
  , PixelRGB 0.5 1 1
  , PixelRGB 0.5 0.5 1
  , PixelRGB 0.75 0.25 0.75
  , PixelRGB 0.628 0.643 0.142
  , PixelRGB 0.255 0.104 0.918
  , PixelRGB 0.116 0.675 0.255
  , PixelRGB 0.941 0.785 0.053
  , PixelRGB 0.704 0.187 0.897
  , PixelRGB 0.931 0.463 0.316
  , PixelRGB 0.998 0.974 0.532
  , PixelRGB 0.099 0.795 0.591
  , PixelRGB 0.211 0.149 0.597
  , PixelRGB 0.495 0.493 0.721
  ]
