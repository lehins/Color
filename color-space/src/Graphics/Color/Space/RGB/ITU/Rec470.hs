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
-- Module      : Graphics.Color.Space.RGB.ITU.Rec470
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.ITU.Rec470
  ( BT470_525
  , C
  , BT470_625
  , D65
  , primaries525
  , primaries625
  , module Graphics.Color.Space
  ) where

import Foreign.Storable
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Illuminant.ITU.Rec470

------------------------------------
-- ITU-R BT.470 (525) --------------
------------------------------------

-- | ITU-R BT.470 (525) color space. Used in NTSC
data BT470_525

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

-- | ITU-R BT.470 (525) color space
instance Elevator e => ColorSpace BT470_525 C e where
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}

-- | ITU-R BT.470 (525) color space
instance RedGreenBlue BT470_525 C where
  gamut = primaries525
  ecctf = fmap (gamma 2.2)
  {-# INLINE ecctf #-}
  dcctf = fmap (igamma 2.2)
  {-# INLINE dcctf #-}

------------------------------------
-- ITU-R BT.470 (625) --------------
------------------------------------

-- | ITU-R BT.470 (625) color space. Used in PAL/SECAM
data BT470_625

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

-- | ITU-R BT.470 (625) color space
instance Elevator e => ColorSpace BT470_625 D65 e where
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}

-- | ITU-R BT.470 (625) color space
instance RedGreenBlue BT470_625 D65 where
  gamut = primaries625
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
primaries525 :: RealFloat e => Gamut rgb i e
primaries525 = Gamut (Primary 0.67 0.33)
                     (Primary 0.21 0.71)
                     (Primary 0.14 0.08)


-- | Primaries for ITU-R BT.470 and BT.601 (625).
--
-- @since 0.1.0
primaries625 :: RealFloat e => Gamut rgb i e
primaries625 = Gamut (Primary 0.64 0.33)
                     (Primary 0.29 0.60)
                     (Primary 0.15 0.06)

