{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( Pixel(Pixel, PixelY, PixelXYZ, PixelRGB, PixelHSI, PixelHSL, PixelHSV,
      PixelCMYK, PixelYCbCr, PixelYA, PixelXYZA, PixelRGBA, PixelHSIA, PixelHSLA,
      PixelHSVA, PixelCMYKA, PixelYCbCrA)
  , liftPixel
  -- * Conversion
  -- ** Color space
  , convertPixel
  , toPixelY
  , toPixelXYZ
  , fromPixelXYZ
  , toPixelBaseSpace
  , fromPixelBaseSpace
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
  -- * sRGB color space
  , pattern PixelSRGB
  , pattern PixelSRGBA
  , SRGB
  , D65
  -- * Adobe RGB color space
  , AdobeRGB
  -- * Re-export of color space
  , module Graphics.Color.Space
  , module Graphics.Color.Space.RGB.Alternative
  , module Graphics.Color.Algebra.Binary
  ) where

import Data.Coerce
import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Algebra.Binary
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.AdobeRGB
import Graphics.Color.Space.RGB.Alternative
import Graphics.Color.Space.RGB.SRGB
import Graphics.Pixel.Internal

-- | Convert a pixel from one color space to any other.
--
-- >>> :set -XTypeApplications
-- >>> px = PixelSRGB @Float 0.0 0.5 1.0
-- >>> px
-- <SRGB:( 0.000000, 0.500000, 1.000000)>
-- >>> convertPixel @AdobeRGB @_ @Word8 px
-- <AdobeRGB:( 71,127,251)>
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
pattern PixelSRGB :: e -> e -> e -> Pixel SRGB e
pattern PixelSRGB r g b = Pixel (SRGB (CM.ColorRGB r g b))
{-# COMPLETE PixelSRGB #-}

-- | Constructor for a pixel in @sRGB@ color space with Alpha channel
--
-- @since 0.1.0
pattern PixelSRGBA :: e -> e -> e -> e -> Pixel (Alpha SRGB) e
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


-- | Constructor for a pixel in RGB color space.
--
-- @since 0.1.0
pattern PixelRGB :: RedGreenBlue cs (i :: k) => e -> e -> e -> Pixel cs e
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


-- | Constructor for a pixel in @YCbCr@.
--
-- @since 0.1.0
pattern PixelYCbCr :: e -> e -> e -> Pixel (YCbCr cs) e
pattern PixelYCbCr y cb cr = Pixel (ColorYCbCr y cb cr)
{-# COMPLETE PixelYCbCr #-}


-- | Constructor for a pixel in RGB color space with Alpha channel
--
-- @since 0.1.0
pattern PixelRGBA :: RedGreenBlue cs i => e -> e -> e -> e -> Pixel (Alpha cs) e
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


-- | Constructor for a pixel in @YCbCr@ with alpha channel.
--
-- @since 0.1.0
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha (YCbCr cs)) e
pattern PixelYCbCrA y cb cr a = Pixel (ColorYCbCrA y cb cr a)
{-# COMPLETE PixelYCbCrA #-}

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

-- | Covert a color space of a pixel into it's alternative representation. Example AdobeRGB to HSI.
--
-- @since 0.1.0
fromPixelBaseSpace ::
     (ColorSpace cs i e, bcs ~ BaseSpace cs, ColorSpace bcs i e) => Pixel bcs e -> Pixel cs e
fromPixelBaseSpace = liftPixel fromBaseSpace
{-# INLINE fromPixelBaseSpace #-}


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
