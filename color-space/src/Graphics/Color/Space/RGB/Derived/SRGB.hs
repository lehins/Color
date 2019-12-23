{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Derived.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Derived.SRGB
  ( SRGB
  , SRGB.primaries
  , SRGB.transfer
  , SRGB.itransfer
  , module Graphics.Color.Space
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.Luma
import qualified Graphics.Color.Space.RGB.SRGB as SRGB


-- | The most common @sRGB@ color space with an arbitrary illuminant
data SRGB (i :: k)

-- | `SRGB` color space (derived)
newtype instance Pixel (SRGB i) e = SRGB (Pixel CM.RGB e)

-- | `SRGB` color space (derived)
deriving instance Eq e => Eq (Pixel (SRGB i) e)
-- | `SRGB` color space (derived)
deriving instance Ord e => Ord (Pixel (SRGB i) e)
-- | `SRGB` color space (derived)
deriving instance Functor (Pixel (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Applicative (Pixel (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Foldable (Pixel (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Traversable (Pixel (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Storable e => Storable (Pixel (SRGB i) e)

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => Show (Pixel (SRGB (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorModel (SRGB (i :: k)) e where
  type Components (SRGB i) e = (e, e, e)
  toComponents = toComponents . unPixelRGB
  {-# INLINE toComponents #-}
  fromComponents = mkPixelRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (SRGB i) i e where
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelY = rgbLuminocity . fmap toRealFloat
  {-# INLINE toPixelY #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}

-- | `SRGB` color space (derived)
instance Illuminant i => RedGreenBlue (SRGB i) i where
  gamut = SRGB.primaries
  ecctf = fmap SRGB.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap SRGB.itransfer
  {-# INLINE dcctf #-}


instance Luma (SRGB i) where
  rWeight = 0.299
  gWeight = 0.587
  bWeight = 0.114
