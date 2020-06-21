{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.Color.Illuminant.ITU.Rec601
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Illuminant.ITU.Rec601
  ( D65
  ) where

import Graphics.Color.Space.Internal

-- | Whitepoint D65 that is so commonly used for sRGB and other color spaces defined by
-- ITU: Rec.601 (525 and 625) and Rec.709 standards. It is slightly different than
-- `Graphics.Color.Illuminant.CIE1931.D65` that is defined by CIE1931 and
-- `Graphics.Color.Illuminant.ITU.Rec470.D65` specified in Rec.470.
--
-- >>> whitePoint :: WhitePoint D65 Float
-- WhitePoint (Chromaticity <CIExyY * D65:( 0.312700, 0.329000)>)
-- >>> whitePointTristimulus :: Color (XYZ D65) Float
-- <XYZ * D65:( 0.950456, 1.000000, 1.089058)>
-- >>> colorTemperature :: CCT D65
-- CCT {unCCT = 6504.0}
--
-- @since 0.1.0
data D65

-- | @[x=0.3127, y=0.3290]@ - /Rec. ITU-R BT.601-7/, /Rec. ITU-R BT.709-6/, /IEC 61966-2-1:1999/
instance Illuminant D65 where
  type Temperature D65 = 6504
  whitePoint = WhitePoint 0.3127 0.3290
