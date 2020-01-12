{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.HSLSpec (spec) where

import Graphics.Color.Model
import Graphics.Color.Model.Common
import Graphics.Color.Model.RGBSpec (rgbs)

import qualified Data.Colour.RGBSpace as Colour
import qualified Data.Colour.RGBSpace.HSL as Colour

instance (Elevator e, Random e) => Arbitrary (Color HSL e) where
  arbitrary = ColorHSL <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "HSL" $ do
    colorModelSpec @HSL @Word "HSL"
    prop "rgb2hsl . hsl2rgb" $ \(rgb :: Color RGB Double) ->
      rgb `epsilonEqColor` hsl2rgb (rgb2hsl rgb)
    prop "hsl2rgb . rgb2hsl" $ \(hsl :: Color HSL Double) ->
      hsl `epsilonEqColor` rgb2hsl (hsl2rgb hsl)
    describe "samples" $ do
      let tol = 2e-3
      describe "rgb2hsl" $ izipWithM_ (epsilonColorIxSpec tol) hsls (rgb2hsl <$> rgbs)
      describe "hsl2rgb" $ izipWithM_ (epsilonColorIxSpec tol) rgbs (hsl2rgb <$> hsls)
    describe "Same as colour package" $ do
      prop "rgb2hsl" $ \ rgb@(ColorRGB r g b) ->
        case Colour.hslView (Colour.RGB r g b) of
          (h, s, l) -> rgb2hsl rgb `epsilonEqColorFloat` ColorH360SL h s l
      prop "hsl2rgb" $ \ hsl@(ColorH360SL h s l) ->
        case Colour.hsl h s l of
          Colour.RGB r g b -> hsl2rgb hsl `epsilonEqColorFloat` ColorRGB r g b


hsls :: [Color HSL Double]
hsls =
  [ ColorH360SL (0 / 0) 0 1
  , ColorH360SL (0 / 0) 0 0.5
  , ColorH360SL (0 / 0) 0 0
  , ColorH360SL 0.0 1 0.5
  , ColorH360SL 60.0 1 0.375
  , ColorH360SL 120.0 1 0.25
  , ColorH360SL 180.0 1 0.75
  , ColorH360SL 240.0 1 0.75
  , ColorH360SL 300.0 0.5 0.5
  , ColorH360SL 61.8 0.638 0.393
  , ColorH360SL 251.1 0.832 0.511
  , ColorH360SL 134.9 0.707 0.396
  , ColorH360SL 49.5 0.893 0.498
  , ColorH360SL 283.7 0.775 0.543
  , ColorH360SL 14.3 0.817 0.624
  , ColorH360SL 56.9 0.991 0.765
  , ColorH360SL 162.4 0.779 0.447
  , ColorH360SL 248.3 0.601 0.373
  , ColorH360SL 240.5 0.29 0.608
  ]
