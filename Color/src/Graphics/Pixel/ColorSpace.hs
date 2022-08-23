{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Pixel.ColorSpace
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Pixel.ColorSpace
  ( Pixel(Pixel, PixelX, PixelY, PixelXYZ, PixelLAB, PixelRGB, PixelHSI, PixelHSL, PixelHSV,
      PixelCMYK, PixelY'CbCr, PixelY', PixelYA, PixelXYZA, PixelLABA, PixelRGBA, PixelHSIA, PixelHSLA,
      PixelHSVA, PixelCMYKA, PixelY'CbCrA, PixelY'A)
  , liftPixel
  , pixelColor
  -- * Conversion
  -- ** Color space
  , convertPixel
  , toPixelY
  , toPixelXYZ
  , fromPixelXYZ
  , toPixelBaseSpace
  , fromPixelBaseSpace
  -- * Grayscale
  , grayscalePixel
  , applyGrayscalePixel
  , replaceGrayscalePixel
  -- ** Color model
  , toPixelBaseModel
  , fromPixelBaseModel
  -- ** Precision
  , toPixel8
  , toPixel16
  , toPixel32
  , toPixel64
  , toPixelF
  , toPixelD
  -- * RGB
  , toPixelLinearRGB
  , fromPixelLinearRGB
  -- ** sRGB color space
  , pattern PixelSRGB
  , pattern PixelSRGBA
  -- ** Luma
  , rgbPixelLuma
  -- * Re-export of color space
  , module Graphics.Color.Space
  , module Graphics.Color.Algebra.Binary
  ) where

import Data.Coerce
import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Algebra.Binary
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Pixel (pattern PixelX)
import Graphics.Pixel.Internal

-- | Convert a pixel from one color space to any other.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> px = PixelSRGB @Float 0.0 0.5 1.0
-- >>> px
-- <SRGB 'NonLinear:( 0.00000000, 0.50000000, 1.00000000)>
-- >>> convertPixel @(AdobeRGB 'NonLinear) @_ @Word8 px
-- <AdobeRGB 'NonLinear:( 71,127,251)>
--
-- @since 0.1.0
convertPixel ::
     forall cs i e cs' i' e' . (ColorSpace cs' i' e', ColorSpace cs i e)
  => Pixel cs' e'
  -> Pixel cs e
convertPixel = liftPixel convert
{-# INLINE convertPixel #-}

-- | Constructor for a pixel in @sRGB@ color space
--
-- @since 0.1.0
pattern PixelSRGB :: e -> e -> e -> Pixel (SRGB l) e
pattern PixelSRGB r g b = Pixel (SRGB (CM.ColorRGB r g b))
{-# COMPLETE PixelSRGB #-}

-- | Constructor for a pixel in @sRGB@ color space with Alpha channel
--
-- @since 0.1.0
pattern PixelSRGBA :: e -> e -> e -> e -> Pixel (Alpha (SRGB l)) e
pattern PixelSRGBA r g b a = Pixel (Alpha (SRGB (CM.ColorRGB r g b)) a)
{-# COMPLETE PixelSRGBA #-}

-- | Constructor for a pixel with Luminance
--
-- @since 0.1.0
pattern PixelY :: e -> Pixel (Y i) e
pattern PixelY y = Pixel (Y y)
{-# COMPLETE PixelY #-}

-- | Constructor for a pixel with Luminance and Alpha channel
--
-- @since 0.1.0
pattern PixelYA :: e -> e -> Pixel (Alpha (Y i)) e
pattern PixelYA y a = Pixel (Alpha (Y y) a)
{-# COMPLETE PixelYA #-}

-- | Constructor for a pixel in @CIE1931 XYZ@ color space
--
-- @since 0.1.0
pattern PixelXYZ :: e -> e -> e -> Pixel (XYZ i) e
pattern PixelXYZ x y z = Pixel (XYZ (V3 x y z))
{-# COMPLETE PixelXYZ #-}


-- | Constructor for a pixel in @CIE1931 XYZ@ color space with Alpha channel
--
-- @since 0.1.0
pattern PixelXYZA :: e -> e -> e -> e -> Pixel (Alpha (XYZ i)) e
pattern PixelXYZA x y z a = Pixel (Alpha (XYZ (V3 x y z)) a)
{-# COMPLETE PixelXYZA #-}


-- | Constructor for a pixel in @CIE1976 LAB@ color space
--
-- @since 0.3.0
pattern PixelLAB :: e -> e -> e -> Pixel (LAB i) e
pattern PixelLAB l' a' b' = Pixel (LAB (V3 l' a' b'))
{-# COMPLETE PixelLAB #-}

-- | Constructor for a pixel in @CIE1976 LAB@ color space with Alpha channel
--
-- @since 0.3.0
pattern PixelLABA :: e -> e -> e -> e -> Pixel (Alpha (LAB i)) e
pattern PixelLABA l' a' b' a = Pixel (Alpha (LAB (V3 l' a' b')) a)
{-# COMPLETE PixelLABA #-}

-- | Constructor for a pixel in RGB color space.
--
-- @since 0.1.0
pattern PixelRGB :: RedGreenBlue cs (i :: k) => e -> e -> e -> Pixel (cs l) e
pattern PixelRGB r g b <- (coerce . unColorRGB . coerce -> V3 r g b) where
        PixelRGB r g b = coerce (mkColorRGB (coerce (V3 r g b)))
{-# COMPLETE PixelRGB #-}

-- | Constructor for a pixel in @HSI@.
--
-- @since 0.1.0
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs) e
pattern PixelHSI h s i = Pixel (ColorHSI h s i)
{-# COMPLETE PixelHSI #-}


-- | Constructor for a pixel in @HSL@.
--
-- @since 0.1.0
pattern PixelHSL :: e -> e -> e -> Pixel (HSL cs) e
pattern PixelHSL h s l = Pixel (ColorHSL h s l)
{-# COMPLETE PixelHSL #-}


-- | Constructor for a pixel in @HSV@.
--
-- @since 0.1.0
pattern PixelHSV :: e -> e -> e -> Pixel (HSV cs) e
pattern PixelHSV h s v = Pixel (ColorHSV h s v)
{-# COMPLETE PixelHSV #-}

-- | Constructor for a pixel in @CMYK@.
--
-- @since 0.1.0
pattern PixelCMYK :: e -> e -> e -> e -> Pixel (CMYK cs) e
pattern PixelCMYK c m y k = Pixel (ColorCMYK c m y k)
{-# COMPLETE PixelCMYK #-}


-- | Constructor for a pixel in @Y'CbCr@.
--
-- @since 0.1.0
pattern PixelY'CbCr :: e -> e -> e -> Pixel (Y'CbCr cs) e
pattern PixelY'CbCr y cb cr = Pixel (ColorY'CbCr y cb cr)
{-# COMPLETE PixelY'CbCr #-}

-- | Constructor for a pixel with Luma (not to be confused with luminance `Y`)
--
-- @since 0.1.4
pattern PixelY' :: e -> Pixel (Y' cs) e
pattern PixelY' y = Pixel (Y' y)
{-# COMPLETE PixelY' #-}

-- | Constructor for a pixel with Luma and Alpha channel (not to be confused with luminance `Y`)
--
-- @since 0.1.4
pattern PixelY'A :: e -> e -> Pixel (Alpha (Y' cs)) e
pattern PixelY'A y a = Pixel (Alpha (Y' y) a)
{-# COMPLETE PixelY'A #-}


-- | Constructor for a pixel in RGB color space with Alpha channel
--
-- @since 0.1.0
pattern PixelRGBA :: RedGreenBlue cs i => e -> e -> e -> e -> Pixel (Alpha (cs l)) e
pattern PixelRGBA r g b a <- (pixelColor -> Alpha (unColorRGB -> CM.ColorRGB r g b) a) where
        PixelRGBA r g b a = Pixel (Alpha (mkColorRGB (CM.ColorRGB r g b)) a)
{-# COMPLETE PixelRGBA #-}


-- | Constructor for a pixel in @HSI@ with alpha channel.
--
-- @since 0.1.0
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha (HSI cs)) e
pattern PixelHSIA h s i a = Pixel (ColorHSIA h s i a)
{-# COMPLETE PixelHSIA #-}

-- | Constructor for a pixel in @HSL@ with alpha channel.
--
-- @since 0.1.0
pattern PixelHSLA :: e -> e -> e -> e -> Pixel (Alpha (HSL cs)) e
pattern PixelHSLA h s l a = Pixel (ColorHSLA h s l a)
{-# COMPLETE PixelHSLA #-}


-- | Constructor for a pixel in @HSV@ with alpha channel.
--
-- @since 0.1.0
pattern PixelHSVA :: e -> e -> e -> e -> Pixel (Alpha (HSV cs)) e
pattern PixelHSVA h s v a = Pixel (ColorHSVA h s v a)
{-# COMPLETE PixelHSVA #-}


-- | Constructor for a pixel in @CMYK@ with alpha channel.
--
-- @since 0.1.0
pattern PixelCMYKA :: e -> e -> e -> e -> e -> Pixel (Alpha (CMYK cs)) e
pattern PixelCMYKA c m y k a = Pixel (ColorCMYKA c m y k a)
{-# COMPLETE PixelCMYKA #-}


-- | Constructor for a pixel in @Y'CbCr@ with alpha channel.
--
-- @since 0.1.0
pattern PixelY'CbCrA :: e -> e -> e -> e -> Pixel (Alpha (Y'CbCr cs)) e
pattern PixelY'CbCrA y cb cr a = Pixel (ColorY'CbCrA y cb cr a)
{-# COMPLETE PixelY'CbCrA #-}

-- | Convert non-linear RGB color space into linear one
--
-- @since 0.2.0
toPixelLinearRGB ::
     (RedGreenBlue cs i, RealFloat e) => Pixel (cs 'NonLinear) e -> Pixel (cs 'Linear) e
toPixelLinearRGB = liftPixel dcctf
{-# INLINE toPixelLinearRGB #-}

-- | Convert linear RGB color space into a non-linear one
--
-- @since 0.2.0
fromPixelLinearRGB ::
     (RedGreenBlue cs i, RealFloat e) => Pixel (cs 'Linear) e -> Pixel (cs 'NonLinear) e
fromPixelLinearRGB = liftPixel ecctf
{-# INLINE fromPixelLinearRGB #-}

-- | Convert an RGB pixel to `Y'` if it has the weights specified with `Luma`.
--
-- @since 0.1.4
rgbPixelLuma ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Pixel (cs 'NonLinear) e'
  -> Pixel (Y' cs) e
rgbPixelLuma = liftPixel rgbLuma

-- | Compute luminance of a pixel color
--
-- @since 0.1.0
toPixelY :: ColorSpace cs i e => Pixel cs e -> Pixel (Y i) e
toPixelY = liftPixel (fmap fromDouble . luminance)
{-# INLINE toPixelY #-}

-- | Convert to CIE1931 XYZ color space, with the same illuminant as the original.
--
-- @since 0.1.0
toPixelXYZ :: (ColorSpace cs i e, Elevator a, RealFloat a) => Pixel cs e -> Pixel (XYZ i) a
toPixelXYZ = liftPixel toColorXYZ
{-# INLINE toPixelXYZ #-}


-- | Convert from CIE1931 XYZ color space, with the same illuminant as the target color
-- space.
--
-- @since 0.1.0
fromPixelXYZ :: (ColorSpace cs i e, Elevator a, RealFloat a) => Pixel (XYZ i) a -> Pixel cs e
fromPixelXYZ = liftPixel fromColorXYZ
{-# INLINE fromPixelXYZ #-}



-- Color Space conversions

-- | Drop all color space information and only keep the values encoded in the fitting
-- color model, which the color space is backed by.
--
-- @since 0.1.0
toPixelBaseModel :: ColorSpace cs i e => Pixel cs e -> Pixel (BaseModel cs) e
toPixelBaseModel = liftPixel toBaseModel
{-# INLINE toPixelBaseModel #-}

-- | Promote a pixel without color space information to a color space that is backed by
-- the fitting color model
--
-- @since 0.1.0
fromPixelBaseModel :: ColorSpace cs i e => Pixel (BaseModel cs) e -> Pixel cs e
fromPixelBaseModel = liftPixel fromBaseModel
{-# INLINE fromPixelBaseModel #-}

-- | Convert pixel in an alternative representation of color space, to its base color
-- space. Example from CMYK to SRGB
--
-- @since 0.1.0
toPixelBaseSpace ::
     (ColorSpace cs i e, bcs ~ BaseSpace cs, ColorSpace bcs i e) => Pixel cs e -> Pixel bcs e
toPixelBaseSpace = liftPixel toBaseSpace
{-# INLINE toPixelBaseSpace #-}

-- | Covert a color space of a pixel into its alternative representation. Example AdobeRGB to HSI.
--
-- @since 0.1.0
fromPixelBaseSpace ::
     (ColorSpace cs i e, bcs ~ BaseSpace cs, ColorSpace bcs i e) => Pixel bcs e -> Pixel cs e
fromPixelBaseSpace = liftPixel fromBaseSpace
{-# INLINE fromPixelBaseSpace #-}

-- | Drop chroma information from a pixel. Same as `grayscale` for `Color`
--
-- @since 0.4.0
grayscalePixel :: ColorSpace cs i e => Pixel cs e -> Pixel X e
grayscalePixel = liftPixel grayscale
{-# INLINE grayscalePixel #-}


-- | Apply a function to grayscale information of a pixel leaving chroma untouched. Same
-- as `applyGrayscale` for `Color`
--
-- @since 0.4.0
applyGrayscalePixel :: ColorSpace cs i e => Pixel cs e -> (Pixel X e -> Pixel X e) -> Pixel cs e
applyGrayscalePixel c f = coerce (applyGrayscale (coerce c) (coerce f))
{-# INLINE applyGrayscalePixel #-}

-- | Replace grayscale information in a pixel leaving chroma untouched. Same as
-- `replaceGrayscale` for `Color`
--
-- @since 0.4.0
replaceGrayscalePixel :: ColorSpace cs i e => Pixel cs e -> Pixel X e -> Pixel cs e
replaceGrayscalePixel c e = coerce (replaceGrayscale (coerce c) (coerce e))
{-# INLINE replaceGrayscalePixel #-}


-- -- | Constructor for a pixel in @sRGB@ color space with 8-bits per channel
-- pattern PixelRGB8 :: Word8 -> Word8 -> Word8 -> Pixel SRGB Word8
-- pattern PixelRGB8 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB8 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 16-bits per channel
-- pattern PixelRGB16 :: Word16 -> Word16 -> Word16 -> Pixel SRGB Word16
-- pattern PixelRGB16 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB16 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bits per channel
-- pattern PixelRGB32 :: Word32 -> Word32 -> Word32 -> Pixel SRGB Word32
-- pattern PixelRGB32 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB32 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 64-bits per channel
-- pattern PixelRGB64 :: Word64 -> Word64 -> Word64 -> Pixel SRGB Word64
-- pattern PixelRGB64 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB64 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bit floating point value per channel
-- pattern PixelRGBF :: Float -> Float -> Float -> Pixel SRGB Float
-- pattern PixelRGBF r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGBF #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bit floating point value per channel
-- pattern PixelRGBD :: Double -> Double -> Double -> Pixel SRGB Double
-- pattern PixelRGBD r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGBD #-}
