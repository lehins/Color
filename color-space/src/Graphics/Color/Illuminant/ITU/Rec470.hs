{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.Color.Illuminant.ITU.Rec470
-- Copyright   : (c) Alexey Kuleshevich 2019
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



-- | Whitepoint C that is used for ITU: Rec.470 (525). It is slightly different than the
-- one defined by CIE1931, thus a separate daclaration in here.
data C


-- | @[x=0.310, y=0.316]@ - /Rec. ITU-R BT.470-7/
instance Illuminant C where
  type Temperature C = 6774
  whitePoint = WhitePoint 0.310 0.316



-- ^ Whitepoint D65 that is used for ITU: Rec.470 (625). It is slightly different than the
-- one defined by CIE1931 and the one in Rec.601, thus a separate declaration in here.
data D65


-- | @[x=0.313, y=0.329]@ - /Rec. ITU-R BT.470-7/
instance Illuminant D65 where
  type Temperature D65 = 6504
  whitePoint = WhitePoint 0.313 0.329
