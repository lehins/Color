{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.ITU.Rec470
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.ITU.Rec470
  ( BT470_525
  , C
  , BT470_625
  , D65
  , primaries525
  , primaries625
  , module Graphics.ColorSpace
  ) where

import Foreign.Storable
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace
import Graphics.ColorModel.Internal

------------------------------------
-- ITU-R BT.470 (525) --------------
------------------------------------

-- | Whitepoint C that is used for ITU: Rec.470 (525). It is slightly different than the
-- one defined by CIE1931, thus a separate daclaration in here.
data C

-- | ITU-R BT.470 (525) color space. Used in NTSC
data BT470_525

-- | @[x=0.310, y=0.316]@ - /Rec. ITU-R BT.470-7/
instance Illuminant C where
  type Temperature C = 6774
  whitePoint = WhitePoint 0.310 0.316


newtype instance Pixel BT470_525 e = BT470_525 (Pixel CM.RGB e)

-- | ITU-R BT.470 (525) color space
deriving instance Eq e => Eq (Pixel BT470_525 e)
-- | ITU-R BT.470 (525) color space
deriving instance Ord e => Ord (Pixel BT470_525 e)
-- | ITU-R BT.470 (525) color space
deriving instance Functor (Pixel BT470_525)
-- | ITU-R BT.470 (525) color space
deriving instance Applicative (Pixel BT470_525)
-- | ITU-R BT.470 (525) color space
deriving instance Foldable (Pixel BT470_525)
-- | ITU-R BT.470 (525) color space
deriving instance Traversable (Pixel BT470_525)
-- | ITU-R BT.470 (525) color space
deriving instance Storable e => Storable (Pixel BT470_525 e)

-- | ITU-R BT.470 (525) color space
instance Elevator e => Show (Pixel BT470_525 e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.470 (525) color space
instance Elevator e => ColorModel BT470_525 e where
  type Components BT470_525 e = (e, e, e)
  toComponents = toComponents . unPixelRGB
  {-# INLINE toComponents #-}
  fromComponents = mkPixelRGB . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | ITU-R BT.470 (525) color space
instance Elevator e => ColorSpace BT470_525 e where
  type BaseColorSpace BT470_525 = BT470_525
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = ("BT.470 Standard" ++)

-- | ITU-R BT.470 (525) color space
instance RedGreenBlue BT470_525 C where
  chromaticity = primaries525
  ecctf = fmap (gamma 2.2)
  {-# INLINE ecctf #-}
  dcctf = fmap (igamma 2.2)
  {-# INLINE dcctf #-}

------------------------------------
-- ITU-R BT.470 (625) --------------
------------------------------------

-- ^ Whitepoint D65 that is used for ITU: Rec.470 (625). It is slightly different than the
-- one defined by CIE1931 and the one in Rec.601, thus a separate declaration in here.
data D65

-- | ITU-R BT.470 (625) color space. Used in PAL/SECAM
data BT470_625

-- | @[x=0.313, y=0.329]@ - /Rec. ITU-R BT.470-7/
instance Illuminant D65 where
  type Temperature D65 = 6504
  whitePoint = WhitePoint 0.313 0.329

newtype instance Pixel BT470_625 e = BT470_625 (Pixel CM.RGB e)

-- | ITU-R BT.470 (625) color space
deriving instance Eq e => Eq (Pixel BT470_625 e)
-- | ITU-R BT.470 (625) color space
deriving instance Ord e => Ord (Pixel BT470_625 e)
-- | ITU-R BT.470 (625) color space
deriving instance Functor (Pixel BT470_625)
-- | ITU-R BT.470 (625) color space
deriving instance Applicative (Pixel BT470_625)
-- | ITU-R BT.470 (625) color space
deriving instance Foldable (Pixel BT470_625)
-- | ITU-R BT.470 (625) color space
deriving instance Traversable (Pixel BT470_625)
-- | ITU-R BT.470 (625) color space
deriving instance Storable e => Storable (Pixel BT470_625 e)

-- | ITU-R BT.470 (625) color space
instance Elevator e => Show (Pixel BT470_625 e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.470 (625) color space
instance Elevator e => ColorModel BT470_625 e where
  type Components BT470_625 e = (e, e, e)
  toComponents = toComponents . unPixelRGB
  {-# INLINE toComponents #-}
  fromComponents = mkPixelRGB . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | ITU-R BT.470 (625) color space
instance Elevator e => ColorSpace BT470_625 e where
  type BaseColorSpace BT470_625 = BT470_625
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = ("BT.470 Standard" ++)

-- | ITU-R BT.470 (625) color space
instance RedGreenBlue BT470_625 D65 where
  chromaticity = primaries625
  ecctf = fmap (gamma 2.8)
  {-# INLINE ecctf #-}
  dcctf = fmap (igamma 2.8)
  {-# INLINE dcctf #-}


gamma :: Floating a => a -> a -> a
gamma p v = v ** p
{-# INLINE gamma #-}

igamma :: Floating a => a -> a -> a
igamma p v = v ** (1 / p)
{-# INLINE igamma #-}

-- | Primaries for ITU-R BT.470 (525).
--
-- @since 0.1.0
primaries525 :: (Illuminant i, RealFloat e) => Chromaticity rgb i e
primaries525 = Chromaticity (Primary 0.67 0.33)
                            (Primary 0.21 0.71)
                            (Primary 0.14 0.08)


-- | Primaries for ITU-R BT.470 and BT.601 (625).
--
-- @since 0.1.0
primaries625 :: (Illuminant i, RealFloat e) => Chromaticity rgb i e
primaries625 = Chromaticity (Primary 0.64 0.33)
                            (Primary 0.29 0.60)
                            (Primary 0.15 0.06)

