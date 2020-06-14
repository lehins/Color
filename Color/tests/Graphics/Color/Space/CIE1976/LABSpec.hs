{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1976.LABSpec (spec) where

import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.CIE.Illuminant as Colour
import qualified Data.Colour.SRGB as Colour
import Graphics.Color.Illuminant.CIE1931 as I2
import qualified Graphics.Color.Illuminant.Wikipedia as W
import Graphics.Color.Space.CIE1976.LAB
import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (LAB (i :: k)) e) where
  arbitrary = ColorLAB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "LAB" $ do
  colorModelSpec @(LAB 'D65) @Word "LAB"
  colorSpaceLenientSpec @(LAB 'D65) @Double 1e-10
  describe "Same as colour package" $ do
    prop "lab2srgb" $ \lab@(ColorLAB l' a' b' :: Color (LAB 'W.D65) Double) ->
      case Colour.toSRGB (Colour.cieLAB Colour.d65 l' a' b') of
        Colour.RGB r g b ->
          (convertColor lab :: Color (SRGB 'W.D65 'NonLinear) Double)
          `epsilonEqColorDouble` ColorRGB r g b
    prop "srgb2xlab" $ \rgb@(ColorRGB r g b :: Color (SRGB 'W.D65 'NonLinear) Double) ->
      case Colour.cieLABView Colour.d65 (Colour.sRGB r g b) of
        (l', a', b') ->
          convertColor rgb `epsilonEqColorDouble` (ColorLAB l' a' b' :: Color (LAB 'W.D65) Double)
