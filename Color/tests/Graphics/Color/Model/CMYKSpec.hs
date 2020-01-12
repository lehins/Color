{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.CMYKSpec (spec) where

import Graphics.Color.Model
import Graphics.Color.Model.Common
import Graphics.Color.Model.RGBSpec ()
import qualified Codec.Picture.Types as JuicyPixels

instance (Elevator e, Random e) => Arbitrary (Color CMYK e) where
  arbitrary =
    ColorCMYK <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator <*>
    arbitraryElevator

spec :: Spec
spec =
  describe "CMYK" $ do
    colorModelSpec @CMYK @Word "CMYK"
    prop "rgb2cmyk . cmyk2rgb" $ \(rgb :: Color RGB Double) ->
      rgb `epsilonEqColor` cmyk2rgb (rgb2cmyk rgb)
    describe "Match JuicyPixels" $ do
      prop "rgb2cmyk" $ \(rgb@(ColorRGB r g b) :: Color RGB Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelRGB8 r g b) of
          JuicyPixels.PixelCMYK8 c m y k ->
            toWord8 <$> rgb2cmyk (toFloat <$> rgb) `approxIntegralColorExpect1` ColorCMYK c m y k
      prop "cmyk2rgb" $ \(cmyk@(ColorCMYK c m y k) :: Color CMYK Word8) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelCMYK8 c m y k) of
          JuicyPixels.PixelRGB8 r g b ->
            toWord8 <$> cmyk2rgb (toFloat <$> cmyk) `approxIntegralColorExpect1` ColorRGB r g b

      prop "cmyk2rgb - 16bit" $ \(cmyk@(ColorCMYK c m y k) :: Color CMYK Word16) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelCMYK16 c m y k) of
          JuicyPixels.PixelRGB16 r g b ->
            -- toWord16 <$> cmyk2rgb (toFloat <$> cmyk) `approxIntegralColorExpect1` ColorRGB r g b
            approxIntegralColorExpect 2 (toWord16 <$> cmyk2rgb (toFloat <$> cmyk)) (ColorRGB r g b)
      prop "rgb2cmyk - 16bit" $ \(rgb@(ColorRGB r g b) :: Color RGB Word16) ->
        case JuicyPixels.convertPixel (JuicyPixels.PixelRGB16 r g b) of
          JuicyPixels.PixelCMYK16 c m y k ->
            toWord16 <$> rgb2cmyk (toFloat <$> rgb) `approxIntegralColorExpect1` ColorCMYK c m y k
