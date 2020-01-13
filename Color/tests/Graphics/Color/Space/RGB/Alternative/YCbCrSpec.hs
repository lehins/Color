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


instance (Elevator e, Random e) => Arbitrary (Color (YCbCr cs) e) where
  arbitrary =
    ColorYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

-- Accurate roundtrip to/from both XYZ and SRGB is not possible due to the nature of the
-- algorithm.
spec :: Spec
spec =
  describe "YCbCr" $ do
    describe "sRGB" $ do
      colorModelSpec @(YCbCr SRGB) @Word "YCbCr"
      colorSpaceCommonSpec @(YCbCr SRGB) @_ @Double $ pure ()
    describe "Derived-sRGB" $ do
      colorModelSpec @(YCbCr (Derived.SRGB D65)) @Word "YCbCr"
      colorSpaceCommonSpec @(YCbCr (Derived.SRGB D65)) @_ @Double $ pure ()
    prop "toColorYCbCr . toColorYCbCr" $ \(rgb :: Color (Derived.SRGB D65) Double) ->
      rgb `epsilonEqColor`
      fromColorYCbCr (toColorYCbCr rgb :: Color (YCbCr (Derived.SRGB D65)) Double)
    prop "toColorYCbCr == srgb2ycbcr" $ \(rgb :: Color SRGB Double) ->
      srgb2ycbcr rgb `epsilonEqColor`
      (toColorYCbCr rgb :: Color (YCbCr SRGB) Double)
    prop "fromColorYCbCr == ycbcr2srgb" $ \(ycbcr :: Color (YCbCr SRGB) Double) ->
      ycbcr2srgb ycbcr `epsilonEqColor` (fromColorYCbCr ycbcr :: Color SRGB Double)
    describe "Match JuicyPixels" $ do
      prop "rgb2ycbcr" $ \(rgb@(ColorRGB r g b) :: Color SRGB Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelRGB8 r g b) of
          JuicyPixels.PixelYCbCr8 y cb cr ->
            (fromBaseSpace rgb :: Color (YCbCr SRGB) Word8)
            `approxIntegralColorExpect1` ColorYCbCr y cb cr
      prop "ycbcr2rgb" $ \(ycbcr@(ColorYCbCr y cb cr) :: Color (YCbCr SRGB) Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelYCbCr8 y cb cr) of
          JuicyPixels.PixelRGB8 r g b ->
            (toBaseSpace ycbcr) `approxIntegralColorExpect1` ColorRGB r g b
