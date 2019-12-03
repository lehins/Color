{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Graphics.ColorSpace.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--

module Graphics.ColorSpace.RGB
  ( -- * Default sRGB color space
    pattern PixelRGB8
  , pattern PixelRGB16
  , pattern PixelRGB32
  , pattern PixelRGB64
  , pattern PixelRGBF
  , pattern PixelRGBD
  , SRGB
  , D65
  -- * Re-exports
  , module Graphics.ColorSpace
  , module Graphics.ColorSpace.RGB.Alternative
  , module Graphics.ColorSpace.RGB.Luma
  ) where

import Graphics.ColorSpace
import Graphics.ColorSpace.RGB.Alternative
import Graphics.ColorSpace.RGB.Luma
import Graphics.ColorSpace.RGB.SRGB (pattern PixelRGB16, pattern PixelRGB32,
                                     pattern PixelRGB64, pattern PixelRGB8,
                                     pattern PixelRGBD, pattern PixelRGBF, SRGB, D65)
