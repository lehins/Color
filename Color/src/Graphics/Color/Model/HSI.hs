{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Model.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.HSI
  ( HSI
  -- * Constructors for an HSI color model.
  , pattern ColorHSI
  , pattern ColorHSIA
  , pattern ColorH360SI
  , Color
  , ColorModel(..)
  , hsi2rgb
  , rgb2hsi
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

-----------
--- HSI ---
-----------

-- | Hue, Saturation and Intensity color model.
data HSI

-- | `HSI` color model
newtype instance Color HSI e = HSI (V3 e)

-- | Constructor for @HSI@.
pattern ColorHSI :: e -> e -> e -> Color HSI e
pattern ColorHSI h s i = HSI (V3 h s i)
{-# COMPLETE ColorHSI #-}


-- | Constructor for @HSI@ with alpha channel.
pattern ColorHSIA :: e -> e -> e -> e -> Color (Alpha HSI) e
pattern ColorHSIA h s i a = Alpha (ColorHSI h s i) a
{-# COMPLETE ColorHSIA #-}

-- | Constructor for an HSI color model. Difference from `ColorHSI` is that channels are
-- restricted to `Double` and the hue is specified in 0 to 360 degree range, rather than 0
-- to 1. Note, that this is not checked.
pattern ColorH360SI :: Fractional e => e -> e -> e -> Color HSI e
pattern ColorH360SI h s i <- ColorHSI ((* 360) -> h) s i where
        ColorH360SI h s i = ColorHSI (h / 360) s i
{-# COMPLETE ColorH360SI #-}


-- | `HSI` color model
deriving instance Eq e => Eq (Color HSI e)
-- | `HSI` color model
deriving instance Ord e => Ord (Color HSI e)
-- | `HSI` color model
deriving instance Functor (Color HSI)
-- | `HSI` color model
deriving instance Applicative (Color HSI)
-- | `HSI` color model
deriving instance Foldable (Color HSI)
-- | `HSI` color model
deriving instance Traversable (Color HSI)
-- | `HSI` color model
deriving instance Storable e => Storable (Color HSI e)

-- | `HSI` color model
instance Elevator e => Show (Color HSI e) where
  showsPrec _ = showsColorModel

-- | `HSI` color model
instance Elevator e => ColorModel HSI e where
  type Components HSI e = (e, e, e)
  toComponents (ColorHSI h s i) = (h, s, i)
  {-# INLINE toComponents #-}
  fromComponents (h, s, i) = ColorHSI h s i
  {-# INLINE fromComponents #-}


hsi2rgb :: (Ord e, Floating e) => Color HSI e -> Color RGB e
hsi2rgb (ColorHSI h' s i) = getRGB (h' * 2 * pi)
  where
    !is = i * s
    !second = i - is
    !pi3 = pi / 3
    getFirst !a !b = i + is * cos a / cos b
    {-# INLINE getFirst #-}
    getThird !v1 !v2 = i + 2 * is + v1 - v2
    {-# INLINE getThird #-}
    getRGB h
      | h < 0 = ColorRGB 0 0 0
      | h < 2 * pi3 =
        let !r = getFirst h (pi3 - h)
            !b = second
            !g = getThird b r
         in ColorRGB r g b
      | h < 4 * pi3 =
        let !g = getFirst (h - 2 * pi3) (h + pi)
            !r = second
            !b = getThird r g
         in ColorRGB r g b
      | h < 2 * pi =
        let !b = getFirst (h - 4 * pi3) (2 * pi - pi3 - h)
            !g = second
            !r = getThird g b
         in ColorRGB r g b
      | otherwise = ColorRGB 0 0 0
    {-# INLINE getRGB #-}
{-# INLINE hsi2rgb #-}


rgb2hsi :: RealFloat e => Color RGB e -> Color HSI e
rgb2hsi (ColorRGB r g b) = ColorHSI h s i
  where
    !h' = atan2 y x
    !h'2pi = h' / (2 * pi)
    !h
      | h' < 0 = h'2pi + 1
      | otherwise = h'2pi
    !s
      | i == 0 = 0
      | otherwise = 1 - min r (min g b) / i
    !i = (r + g + b) / 3
    !x = (2 * r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
{-# INLINE rgb2hsi #-}
