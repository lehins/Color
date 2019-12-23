{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.HSVSpec (spec) where

import Graphics.Color.Model.Common
import Graphics.Color.Model.HSV
import Graphics.Color.Model.RGB
import Graphics.Color.Model.RGBSpec (rgbs)

instance (Elevator e, Random e) => Arbitrary (Color HSV e) where
  arbitrary = ColorHSV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSV" $ do
    colorModelSpec @HSV @Word
    prop "rgb2hsv . hsv2rgb" $ \(rgb :: Color RGB Double) ->
      rgb `epsilonEqColor` hsv2rgb (rgb2hsv rgb)
    prop "hsv2rgb . rgb2hsv" $ \(hsv :: Color HSV Double) ->
      hsv `epsilonEqColor` rgb2hsv (hsv2rgb hsv)
    describe "samples" $ do
      let tol = 1e-3
      describe "rgb2hsv" $ izipWithM_ (epsilonColorIxSpec tol) hsvs (rgb2hsv <$> rgbs)
      describe "hsv2rgb" $ izipWithM_ (epsilonColorIxSpec tol) rgbs (hsv2rgb <$> hsvs)


hsvs :: [Color HSV Double]
hsvs =
  [ ColorH360SV (0/0) 0 1
  , ColorH360SV (0/0) 0 0.5
  , ColorH360SV (0/0) 0 0
  , ColorH360SV 0.0 1 1
  , ColorH360SV 60.0 1 0.75
  , ColorH360SV 120.0 1 0.5
  , ColorH360SV 180.0 0.5 1
  , ColorH360SV 240.0 0.5 1
  , ColorH360SV 300.0 0.667 0.75
  , ColorH360SV 61.8 0.779 0.643
  , ColorH360SV 251.1 0.887 0.918
  , ColorH360SV 134.9 0.828 0.675
  , ColorH360SV 49.5 0.944 0.941
  , ColorH360SV 283.7 0.792 0.897
  , ColorH360SV 14.3 0.661 0.931
  , ColorH360SV 56.9 0.467 0.998
  , ColorH360SV 162.4 0.875 0.795
  , ColorH360SV 248.3 0.75 0.597
  , ColorH360SV 240.5 0.316 0.721
  ]
