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
    pattern ColorRGB8
  , pattern ColorRGB16
  , pattern ColorRGB32
  , pattern ColorRGB64
  , pattern ColorRGBF
  , pattern ColorRGBD
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
import Graphics.Color.Space.RGB.SRGB (pattern ColorRGB16, pattern ColorRGB32,
                                      pattern ColorRGB64, pattern ColorRGB8,
                                      pattern ColorRGBD, pattern ColorRGBF, SRGB, D65)
