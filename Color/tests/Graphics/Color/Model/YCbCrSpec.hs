{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.YCbCrSpec (spec) where

import Graphics.Color.Model
import Graphics.Color.Model.Common
import Graphics.Color.Model.RGBSpec ()
import qualified Codec.Picture.Types as JuicyPixels

instance (Elevator e, Random e) => Arbitrary (Color YCbCr e) where
  arbitrary =
    ColorYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "YCbCr" $ do
    colorModelSpec @YCbCr @Word "YCbCr"
    -- prop "rgb2ycbcr . ycbcr2rgb" $ \(rgb :: Color RGB Double) ->
    --   rgb `epsilonEqColor` ycbcr2rgb (rgb2ycbcr rgb)
    -- describe "Match JuicyPixels" $ do
    --   prop "rgb2ycbcr" $ \(rgb@(ColorRGB r g b) :: Color RGB Word8) ->
    --     case JuicyPixels.convertPixel (JuicyPixels.PixelRGB8 r g b) of
    --       JuicyPixels.PixelYCbCr8 y cb cr ->
    --         toWord8 <$> rgb2ycbcr (toFloat <$> rgb) `approxIntegralColorExpect1` ColorYCbCr y cb cr
    --   prop "ycbcr2rgb" $ \(ycbcr@(ColorYCbCr y cb cr) :: Color YCbCr Word8) ->
    --     case JuicyPixels.convertPixel (JuicyPixels.PixelYCbCr8 y cb cr) of
    --       JuicyPixels.PixelRGB8 r g b ->
    --         toWord8 <$> ycbcr2rgb (toFloat <$> ycbcr) `approxIntegralColorExpect1` ColorRGB r g b
