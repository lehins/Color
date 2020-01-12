{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.Color.Illuminant.ICC.PCS
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Illuminant.ICC.PCS
  ( D50
  ) where

import Graphics.Color.Space.Internal

-- | This is an approximation of CIE1931 `Graphics.ColorSpace.Illuminant.CIE1931.D50`
-- white point defined in ICC PCS. Useful for chromatic adaptation.
--
-- >>> whitePoint :: WhitePoint D50 Double
-- WhitePoint (Chromaticity <CIExyY * D50:( 0.345702914919, 0.358538596680)>)
-- >>> whitePointTristimulus :: Color (XYZ D50) Float
-- <XYZ * D50:( 0.964200, 1.000000, 0.824900)>
-- >>> colorTemperature :: CCT D50
-- CCT {unCCT = 5003.0}
--
-- @since 0.1.0
data D50

-- | Tristimulus @[X=0.9642, Y=1.0000, Z=0.8249]@ - /IEC 61966-2-1:1999/, /ICC PCS/
instance Illuminant D50 where
  type Temperature D50 = 5003
  whitePoint = WhitePoint 0.345702914919 0.358538596680
