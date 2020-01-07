{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.RGBSpec (spec, rgbs) where

import Graphics.Color.Model
import Graphics.Color.Model.Common

instance (Elevator e, Random e) => Arbitrary (Color RGB e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec =
  describe "RGB" $
    colorModelSpec @RGB @Word


rgbs :: [Color RGB Double]
rgbs =
  [ ColorRGB 1 1 1
  , ColorRGB 0.5 0.5 0.5
  , ColorRGB 0 0 0
  , ColorRGB 1 0 0
  , ColorRGB 0.75 0.75 0
  , ColorRGB 0 0.5 0
  , ColorRGB 0.5 1 1
  , ColorRGB 0.5 0.5 1
  , ColorRGB 0.75 0.25 0.75
  , ColorRGB 0.628 0.643 0.142
  , ColorRGB 0.255 0.104 0.918
  , ColorRGB 0.116 0.675 0.255
  , ColorRGB 0.941 0.785 0.053
  , ColorRGB 0.704 0.187 0.897
  , ColorRGB 0.931 0.463 0.316
  , ColorRGB 0.998 0.974 0.532
  , ColorRGB 0.099 0.795 0.591
  , ColorRGB 0.211 0.149 0.597
  , ColorRGB 0.495 0.493 0.721
  ]
