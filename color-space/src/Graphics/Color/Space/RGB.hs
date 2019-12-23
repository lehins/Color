{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Graphics.Color.Space.RGB
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--

module Graphics.Color.Space.RGB
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
  , module Graphics.Color.Space
  , module Graphics.Color.Space.RGB.Alternative
  , module Graphics.Color.Space.RGB.Luma
  ) where

import Graphics.Color.Space
import Graphics.Color.Space.RGB.Alternative
import Graphics.Color.Space.RGB.Luma
import Graphics.Color.Space.RGB.SRGB (pattern PixelRGB16, pattern PixelRGB32,
                                      pattern PixelRGB64, pattern PixelRGB8,
                                      pattern PixelRGBD, pattern PixelRGBF, SRGB, D65)
