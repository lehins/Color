{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Pixel
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Pixel
  ( Pixel(Pixel, PixelY, PixelRGB, PixelHSI, PixelHSL, PixelHSV,
      PixelCMYK, PixelYCbCr, PixelYA, PixelRGBA, PixelHSIA, PixelHSLA,
      PixelHSVA, PixelCMYKA, PixelYCbCrA)
  , liftPixel
  -- * Elevation
  , toPixel8
  , toPixel16
  , toPixel32
  , toPixel64
  , toPixelF
  , toPixelD
  , module Graphics.Color.Model
  , module Graphics.Color.Algebra.Binary
  ) where

import Graphics.Color.Algebra.Binary
import Graphics.Color.Model
import Graphics.Pixel.Internal

-- | Constructor for a grayscale pixel with single channel.
--
-- @since 0.1.0
pattern PixelY :: e -> Pixel Y e
pattern PixelY y = Pixel (ColorY y)
{-# COMPLETE PixelY #-}

-- | Constructor for a pixel with @RGB@ color model.
--
-- @since 0.1.0
pattern PixelRGB :: e -> e -> e -> Pixel RGB e
pattern PixelRGB r g b = Pixel (ColorRGB r g b)
{-# COMPLETE PixelRGB #-}

-- | Constructor for Pixel with @HSI@ color model.
--
-- @since 0.1.0
pattern PixelHSI :: e -> e -> e -> Pixel HSI e
pattern PixelHSI h s i = Pixel (ColorHSI h s i)
{-# COMPLETE PixelHSI #-}

-- | Constructor for Pixel with @HSL@ color model.
--
-- @since 0.1.0
pattern PixelHSL :: e -> e -> e -> Pixel HSL e
pattern PixelHSL h s l = Pixel (ColorHSL h s l)
{-# COMPLETE PixelHSL #-}

-- | Constructor for Pixel with @HSV@ color model.
--
-- @since 0.1.0
pattern PixelHSV :: e -> e -> e -> Pixel HSV e
pattern PixelHSV h s v = Pixel (ColorHSV h s v)
{-# COMPLETE PixelHSV #-}

-- | Constructor for a pixel with @CMYK@ color model.
--
-- @since 0.1.0
pattern PixelCMYK :: e -> e -> e -> e -> Pixel CMYK e
pattern PixelCMYK c m y k = Pixel (ColorCMYK c m y k)
{-# COMPLETE PixelCMYK #-}

-- | Constructor for a pixel with @YCbCr@ color model.
--
-- @since 0.1.0
pattern PixelYCbCr :: e -> e -> e -> Pixel YCbCr e
pattern PixelYCbCr y cb cr = Pixel (ColorYCbCr y cb cr)
{-# COMPLETE PixelYCbCr #-}


-- | Constructor for a grayscale pixel with a transparency channel.
--
-- @since 0.1.0
pattern PixelYA :: e -> e -> Pixel (Alpha Y) e
pattern PixelYA y a = Pixel (Alpha (ColorY y) a)
{-# COMPLETE PixelYA #-}

-- | Constructor for a pixel with @RGB@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelRGBA :: e -> e -> e -> e -> Pixel (Alpha RGB) e
pattern PixelRGBA r g b a = Pixel (Alpha (ColorRGB r g b) a)
{-# COMPLETE PixelRGBA #-}

-- | Constructor for a pixel with @HSI@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha HSI) e
pattern PixelHSIA h s i a = Pixel (Alpha (ColorHSI h s i) a)
{-# COMPLETE PixelHSIA #-}

-- | Constructor for a pixel with @HSL@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelHSLA :: e -> e -> e -> e -> Pixel (Alpha HSL) e
pattern PixelHSLA h s l a = Pixel (Alpha (ColorHSL h s l) a)
{-# COMPLETE PixelHSLA #-}

-- | Constructor for a pixel with @HSV@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelHSVA :: e -> e -> e -> e -> Pixel (Alpha HSV) e
pattern PixelHSVA h s v a = Pixel (Alpha (ColorHSV h s v) a)
{-# COMPLETE PixelHSVA #-}


-- | Constructor for a pixel with @CMYK@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelCMYKA :: e -> e -> e -> e -> e -> Pixel (Alpha CMYK) e
pattern PixelCMYKA c m y k a = Pixel (Alpha (ColorCMYK c m y k) a)
{-# COMPLETE PixelCMYKA #-}

-- | Constructor for a pixel with @YCbCr@ color model and Alpha channel.
--
-- @since 0.1.0
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha YCbCr) e
pattern PixelYCbCrA y cb cr a = Pixel (Alpha (ColorYCbCr y cb cr) a)
{-# COMPLETE PixelYCbCrA #-}

