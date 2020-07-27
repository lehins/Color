{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Derived.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Derived.SRGB
  ( SRGB
  , module Graphics.Color.Space
  ) where

import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.Luma
import qualified Graphics.Color.Space.RGB.SRGB as SRGB


-- | The most common @sRGB@ color space with an arbitrary illuminant
data SRGB (i :: k) (l :: Linearity)

-- | `SRGB` color space (derived)
newtype instance Color (SRGB i l) e = SRGB (Color CM.RGB e)

-- | `SRGB` color space (derived)
deriving instance Eq e => Eq (Color (SRGB i l) e)
-- | `SRGB` color space (derived)
deriving instance Ord e => Ord (Color (SRGB i l) e)
-- | `SRGB` color space (derived)
deriving instance Functor (Color (SRGB i l))
-- | `SRGB` color space (derived)
deriving instance Applicative (Color (SRGB i l))
-- | `SRGB` color space (derived)
deriving instance Foldable (Color (SRGB i l))
-- | `SRGB` color space (derived)
deriving instance Traversable (Color (SRGB i l))
-- | `SRGB` color space (derived)
deriving instance Storable e => Storable (Color (SRGB i l) e)

-- | `SRGB` color space (derived)
instance (Typeable l, Illuminant i, Elevator e) => Show (Color (SRGB (i :: k) l) e) where
  showsPrec _ = showsColorModel

-- | `SRGB` color space (derived)
instance (Typeable l, Illuminant i, Elevator e) => ColorModel (SRGB (i :: k) l) e where
  type Components (SRGB i l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `SRGB` linear color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (SRGB i 'Linear) i e where
  type BaseModel (SRGB i 'Linear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgbLinear
  {-# INLINE fromColorXYZ #-}


-- | `SRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (SRGB i 'NonLinear) i e where
  type BaseModel (SRGB i 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | `SRGB` color space (derived)
instance Illuminant i => RedGreenBlue (SRGB i) i where
  gamut = coerceGamut (gamut @_ @SRGB.SRGB)
  transfer = transfer @_ @SRGB.SRGB
  {-# INLINE transfer #-}
  itransfer = itransfer @_ @SRGB.SRGB
  {-# INLINE itransfer #-}


instance Luma (SRGB i) where
  rWeight = 0.299
  gWeight = 0.587
  bWeight = 0.114
