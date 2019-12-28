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
newtype instance Color (SRGB i) e = SRGB (Color CM.RGB e)

-- | `SRGB` color space (derived)
deriving instance Eq e => Eq (Color (SRGB i) e)
-- | `SRGB` color space (derived)
deriving instance Ord e => Ord (Color (SRGB i) e)
-- | `SRGB` color space (derived)
deriving instance Functor (Color (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Applicative (Color (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Foldable (Color (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Traversable (Color (SRGB i))
-- | `SRGB` color space (derived)
deriving instance Storable e => Storable (Color (SRGB i) e)

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => Show (Color (SRGB (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorModel (SRGB (i :: k)) e where
  type Components (SRGB i) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (SRGB i) i e where
  type BaseModel (SRGB i) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  toColorY = rgbLuminocity . fmap toRealFloat
  {-# INLINE toColorY #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

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
