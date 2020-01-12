{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
import Graphics.Color.Space.RGB.Derived.SRGB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (SRGB (i :: k)) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(SRGB 'D65) @Word "SRGB"
  colorSpaceSpec @(SRGB 'D65) @_ @Float
  describe "Same as colour package" $ do
    prop "xyz2srgb" $ \xyz@(ColorXYZ x y z :: Color (XYZ 'W.D65) Double) ->
      case Colour.toSRGB (Colour.cieXYZ x y z) of
        Colour.RGB r g b ->
          (fromColorXYZ xyz :: Color (SRGB 'W.D65) Double) `epsilonEqColorDouble` ColorRGB r g b
    prop "xyz2linearsrgb" $ \xyz@(ColorXYZ x y z :: Color (XYZ 'W.D65) Double) ->
      case Colour.toRGB (Colour.cieXYZ x y z) of
        Colour.RGB r g b ->
          dcctf (fromColorXYZ xyz :: Color (SRGB 'W.D65) Double) `epsilonEqColorDouble`
          ColorRGB r g b
    prop "srgb2xyz" $ \rgb@(ColorRGB r g b :: Color (SRGB 'W.D65) Double) ->
      case Colour.cieXYZView (Colour.sRGB r g b) of
        (x, y, z) ->
          toColorXYZ rgb `epsilonEqColorDouble` ColorXYZ x y z
    prop "linersrgb2xyz" $ \rgb@(ColorRGB r g b :: Color (SRGB 'W.D65) Double) ->
      case Colour.cieXYZView (Colour.rgb r g b) of
        (x, y, z) ->
          toColorXYZ (ecctf rgb) `epsilonEqColorDouble` ColorXYZ x y z
