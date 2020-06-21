{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.Color.Illuminant.ITU.Rec470
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Illuminant.ITU.Rec470
  ( C
  , D65
  ) where

import Graphics.Color.Space.Internal



-- | Whitepoint C that is used for ITU: Rec.470 (525). It is slightly different than
-- `Graphics.Color.Illuminant.CIE1931.C` that is defined by CIE1931, thus a separate
-- declaration in here.
--
-- >>> whitePoint :: WhitePoint C Float
-- WhitePoint (Chromaticity {chromaticityCIExyY = <CIExyY * C:( 0.31000000, 0.31600000)>})
-- >>> whitePointTristimulus :: Color (XYZ C) Float
-- <XYZ * C:( 0.98101264, 1.00000000, 1.18354420)>
-- >>> colorTemperature :: CCT C
-- CCT {unCCT = 6774.0}
--
-- @since 0.1.0
data C


-- | @[x=0.310, y=0.316]@ - /Rec. ITU-R BT.470-7/
instance Illuminant C where
  type Temperature C = 6774
  whitePoint = WhitePoint 0.310 0.316



-- | Whitepoint D65 that is used for ITU: Rec.470 (625). It is slightly different than
-- `Graphics.Color.Illuminant.CIE1931.D65` that is defined by CIE1931 and
-- `Graphics.Color.Illuminant.ITU.Rec601.D65` specified in Rec.601, thus a separate
-- declaration in here.
--
-- >>> whitePoint :: WhitePoint D65 Float
-- WhitePoint (Chromaticity {chromaticityCIExyY = <CIExyY * D65:( 0.31300000, 0.32900000)>})
-- >>> whitePointTristimulus :: Color (XYZ D65) Float
-- <XYZ * D65:( 0.95136780, 1.00000000, 1.08814610)>
-- >>> colorTemperature :: CCT D65
-- CCT {unCCT = 6504.0}
--
-- @since 0.1.0
data D65


-- | @[x=0.313, y=0.329]@ - /Rec. ITU-R BT.470-7/
instance Illuminant D65 where
  type Temperature D65 = 6504
  whitePoint = WhitePoint 0.313 0.329
