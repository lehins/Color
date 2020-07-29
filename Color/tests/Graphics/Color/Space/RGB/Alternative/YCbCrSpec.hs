{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.Alternative.YCbCrSpec (spec) where

import qualified Codec.Picture.Types as JuicyPixels
import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.SRGB
import qualified Graphics.Color.Space.RGB.Derived.SRGB as Derived
import Graphics.Color.Space.RGB.Alternative.YCbCr
import Graphics.Color.Space.RGB.SRGBSpec ()
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()


instance (Elevator e, Random e) => Arbitrary (Color (Y'CbCr cs) e) where
  arbitrary =
    ColorY'CbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

-- Accurate roundtrip to/from both XYZ and SRGB is not possible due to the nature of the
-- algorithm.
spec :: Spec
spec =
  describe "Y'CbCr" $ do
    describe "sRGB" $ do
      colorModelSpec @(Y'CbCr SRGB) @Word "Y'CbCr"
      colorSpaceCommonSpec @(Y'CbCr SRGB) @Double $ pure ()
    describe "Derived-sRGB" $ do
      colorModelSpec @(Y'CbCr (Derived.SRGB D65)) @Word "Y'CbCr"
      colorSpaceCommonSpec @(Y'CbCr (Derived.SRGB D65)) @Double $ pure ()
    prop "toColorY'CbCr . toColorY'CbCr" $ \(rgb :: Color (Derived.SRGB D65 'NonLinear) Double) ->
      rgb `epsilonEqColor`
      fromColorY'CbCr (toColorY'CbCr rgb :: Color (Y'CbCr (Derived.SRGB D65)) Double)
    prop "toColorY'CbCr == srgb2ycbcr" $ \(rgb :: Color (SRGB 'NonLinear) Double) ->
      srgb2ycbcr rgb `epsilonEqColor`
      (toColorY'CbCr rgb :: Color (Y'CbCr SRGB) Double)
    prop "fromColorY'CbCr == ycbcr2srgb" $ \(ycbcr :: Color (Y'CbCr SRGB) Double) ->
      ycbcr2srgb ycbcr `epsilonEqColor` (fromColorY'CbCr ycbcr :: Color (SRGB 'NonLinear) Double)
    describe "Match JuicyPixels" $ do
      prop "rgb2ycbcr" $ \(rgb@(ColorRGB r g b) :: Color (SRGB 'NonLinear) Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelRGB8 r g b) of
          JuicyPixels.PixelYCbCr8 y cb cr ->
            (fromBaseSpace rgb :: Color (Y'CbCr SRGB) Word8)
            `approxIntegralColorExpect1` ColorY'CbCr y cb cr
      prop "ycbcr2rgb" $ \(ycbcr@(ColorY'CbCr y cb cr) :: Color (Y'CbCr SRGB) Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelYCbCr8 y cb cr) of
          JuicyPixels.PixelRGB8 r g b ->
            toBaseSpace ycbcr `approxIntegralColorExpect1` ColorRGB r g b
