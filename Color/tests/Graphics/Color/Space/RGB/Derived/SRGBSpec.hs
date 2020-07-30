{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Derived.SRGBSpec (spec) where

import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.SRGB as Colour
import qualified Data.Colour.SRGB.Linear as Colour
import Graphics.Color.Illuminant.CIE1931
import qualified Graphics.Color.Illuminant.Wikipedia as W
import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.Derived.SRGB as D

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (D.SRGB (i :: k) l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(D.SRGB 'D65 'NonLinear) @Word "SRGB CIE1931 'D65 'NonLinear"
  colorSpaceSpec @(D.SRGB 'D65 'NonLinear) @Float
  colorModelSpec @(D.SRGB 'D55 'Linear) @Int "SRGB CIE1931 'D55 'Linear"
  colorSpaceSpec @(D.SRGB 'D55 'Linear) @Double
  describe "Same as colour package" $ do
    prop "xyz2srgb" $ \xyz@(ColorXYZ x y z :: Color (XYZ 'W.D65) Double) ->
      case Colour.toSRGB (Colour.cieXYZ x y z) of
        Colour.RGB r g b ->
          (fromColorXYZ xyz :: Color (D.SRGB 'W.D65 'NonLinear) Double)
          `epsilonEqColorDouble`
          ColorRGB r g b
    prop "xyz2linearsrgb" $ \xyz@(ColorXYZ x y z :: Color (XYZ 'W.D65 ) Double) ->
      case Colour.toRGB (Colour.cieXYZ x y z) of
        Colour.RGB r g b ->
          dcctf (fromColorXYZ xyz :: Color (D.SRGB 'W.D65 'NonLinear) Double) `epsilonEqColorDouble`
          ColorRGB r g b
    prop "srgb2xyz" $ \rgb@(ColorRGB r g b :: Color (D.SRGB 'W.D65 'NonLinear) Double) ->
      case Colour.cieXYZView (Colour.sRGB r g b) of
        (x, y, z) ->
          toColorXYZ rgb `epsilonEqColorDouble` ColorXYZ x y z
    prop "linersrgb2xyz" $ \rgb@(ColorRGB r g b :: Color (D.SRGB 'W.D65 'Linear) Double) ->
      case Colour.cieXYZView (Colour.rgb r g b) of
        (x, y, z) ->
          toColorXYZ (ecctf rgb) `epsilonEqColorDouble` ColorXYZ x y z
