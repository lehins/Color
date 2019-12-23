{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Graphics.Color.Pixel
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Pixel
  ( Pixel(..)
  , convertPixel
  -- * sRGB color space
  , SRGB
  , pattern PixelRGB
  , pattern PixelRGBA
  , pattern PixelHSI
  , pattern PixelHSIA
  , pattern PixelYCbCr
  , pattern PixelYCbCrA
  , module Graphics.Color.Model
  , module Graphics.Color.Space
  ) where

import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Model
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.SRGB
import Graphics.Color.Space.RGB.Alternative

-- | One of the most common places for color to be used is in imaging, where each pixel
-- has a specific color. This is a zero cost newtype wrapper around a `Color`.
newtype Pixel cs e = Pixel
  { pixelColor :: Color cs e
  }

convertPixel :: (ColorSpace cs' i' e', ColorSpace cs i e) => Pixel cs' e' -> Pixel cs e
convertPixel = Pixel . convert . pixelColor
{-# INLINE convertPixel #-}


-- | Constructor for a pixel in @sRGB@ color space
pattern PixelRGB :: e -> e -> e -> Pixel SRGB e
pattern PixelRGB r g b = Pixel (SRGB (CM.ColorRGB r g b))
{-# COMPLETE PixelRGB #-}

-- | Constructor for a pixel in @sRGB@ color space with Alpha channel
pattern PixelRGBA :: e -> e -> e -> e -> Pixel (Alpha SRGB) e
pattern PixelRGBA r g b a = Pixel (Alpha (SRGB (CM.ColorRGB r g b)) a)
{-# COMPLETE PixelRGBA #-}


-- | Constructor for @HSI@.
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs) e
pattern PixelHSI h s i = Pixel (ColorHSI h s i)
{-# COMPLETE PixelHSI #-}


-- | Constructor for @HSI@ with alpha channel.
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha (HSI cs)) e
pattern PixelHSIA h s i a = Pixel (ColorHSIA h s i a)
{-# COMPLETE PixelHSIA #-}



-- | Constructor for @YCbCr@.
pattern PixelYCbCr :: e -> e -> e -> Pixel (YCbCr cs) e
pattern PixelYCbCr y cb cr = Pixel (ColorYCbCr y cb cr)
{-# COMPLETE PixelYCbCr #-}


-- | Constructor for @YCbCr@ with alpha channel.
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha (YCbCr cs)) e
pattern PixelYCbCrA y cb cr a = Pixel (ColorYCbCrA y cb cr a)
{-# COMPLETE PixelYCbCrA #-}


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
