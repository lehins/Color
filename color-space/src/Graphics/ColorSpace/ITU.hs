{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.ColorSpace.ITU
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.ITU
  ( ITU(..)
  ) where

import Graphics.ColorSpace.Internal

-- | International Telecommunication Union - Radiocommunication Sector (ITU-R)
data ITU = D65
  -- ^ Whitepoint D65 that is so commonly used for sRGB and other related to Rec.601 and
  -- Rec.709 standards. It is slightly different than the one defined by CIE1931, thus a
  -- separate daclaration in here.

-- | @[x=0.3127, y=0.3290]@ - /IEC 61966-2-1:1999/
instance Illuminant 'D65 where whitePoint = WhitePoint 0.3127 0.3290
