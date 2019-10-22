{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorModel.HSLSpec (spec) where

import Graphics.ColorModel.Common
import Graphics.ColorModel.HSL
import Graphics.ColorModel.RGB
import Graphics.ColorModel.RGBSpec (rgbs)

instance (Elevator e, Random e) => Arbitrary (Pixel HSL e) where
  arbitrary = PixelHSL <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSL" $ do
    colorModelSpec @HSL @Word
    prop "rgb2hsl . hsl2rgb" $ \(rgb :: Pixel RGB Double) ->
      rgb `epsilonEqPixel` hsl2rgb (rgb2hsl rgb)
    prop "hsl2rgb . rgb2hsl" $ \(hsl :: Pixel HSL Double) ->
      hsl `epsilonEqPixel` rgb2hsl (hsl2rgb hsl)
    describe "samples" $ do
      let tol = 2e-3
      describe "rgb2hsl" $ izipWithM_ (epsilonPixelIxSpec tol) hsls (rgb2hsl <$> rgbs)
      describe "hsl2rgb" $ izipWithM_ (epsilonPixelIxSpec tol) rgbs (hsl2rgb <$> hsls)


hsls :: [Pixel HSL Double]
hsls =
  [ PixelH360SL (0 / 0) 0 1
  , PixelH360SL (0 / 0) 0 0.5
  , PixelH360SL (0 / 0) 0 0
  , PixelH360SL 0.0 1 0.5
  , PixelH360SL 60.0 1 0.375
  , PixelH360SL 120.0 1 0.25
  , PixelH360SL 180.0 1 0.75
  , PixelH360SL 240.0 1 0.75
  , PixelH360SL 300.0 0.5 0.5
  , PixelH360SL 61.8 0.638 0.393
  , PixelH360SL 251.1 0.832 0.511
  , PixelH360SL 134.9 0.707 0.396
  , PixelH360SL 49.5 0.893 0.498
  , PixelH360SL 283.7 0.775 0.543
  , PixelH360SL 14.3 0.817 0.624
  , PixelH360SL 56.9 0.991 0.765
  , PixelH360SL 162.4 0.779 0.447
  , PixelH360SL 248.3 0.601 0.373
  , PixelH360SL 240.5 0.29 0.608
  ]
