{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.HSISpec (spec) where

import Graphics.Color.Model.Common
import Graphics.Color.Model.HSI
import Graphics.Color.Model.RGB
import Graphics.Color.Model.RGBSpec (rgbs)

instance (Elevator e, Random e) => Arbitrary (Color HSI e) where
  arbitrary = ColorHSI <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSI" $ do
    colorModelSpec @HSI @Word
    prop "rgb2hsi . hsi2rgb" $ \(rgb :: Color RGB Double) ->
      rgb `epsilonEqColor` hsi2rgb (rgb2hsi rgb)
    prop "hsi2rgb . rgb2hsi" $ \(hsi :: Color HSI Double) ->
      hsi `epsilonEqColor` rgb2hsi (hsi2rgb hsi)
    describe "samples" $ do
      let tol = 1e-3
      describe "rgb2hsi" $ izipWithM_ (epsilonColorIxSpec tol) hsis (rgb2hsi <$> rgbs)
      describe "hsi2rgb" $ izipWithM_ (epsilonColorIxSpec tol) rgbs (hsi2rgb <$> hsis)


hsis :: [Color HSI Double]
hsis =
  [ ColorH360SI 0 0 1
  , ColorH360SI 0 0 0.5
  , ColorH360SI 0 0 0
  , ColorH360SI 0.0 1 0.333
  , ColorH360SI 60.0 1 0.5
  , ColorH360SI 120.0 1 0.167
  , ColorH360SI 180.0 0.4 0.833
  , ColorH360SI 240.0 0.25 0.667
  , ColorH360SI 300.0 0.571 0.583
  , ColorH360SI 61.5 0.699 0.471
  , ColorH360SI 250.0 0.756 0.426
  , ColorH360SI 133.8 0.667 0.349
  , ColorH360SI 50.5 0.911 0.593
  , ColorH360SI 284.8 0.686 0.596
  , ColorH360SI 13.2 0.446 0.57
  , ColorH360SI 57.4 0.363 0.835
  , ColorH360SI 163.4 0.8 0.495
  , ColorH360SI 247.3 0.533 0.319
  , ColorH360SI 240.4 0.135 0.57
  ]
