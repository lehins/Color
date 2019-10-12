{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.HSI
  ( HSI
  , Pixel(..)
  , hsi2rgb
  , rgb2hsi
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB

-----------
--- HSI ---
-----------

-- | Hue, Saturation and Intensity color space.
data HSI

-- | `HSI` color model
data instance Pixel HSI e = PixelHSI !e !e !e
-- | `HSI` color model
deriving instance Eq e => Eq (Pixel HSI e)
-- | `HSI` color model
deriving instance Ord e => Ord (Pixel HSI e)

-- | `HSI` color model
instance Elevator e => Show (Pixel HSI e) where
  showsPrec _ = showsColorModel

-- | `HSI` color model
instance Elevator e => ColorModel HSI e where
  type Components HSI e = (e, e, e)
  toComponents (PixelHSI h s i) = (h, s, i)
  {-# INLINE toComponents #-}
  fromComponents (h, s, i) = PixelHSI h s i
  {-# INLINE fromComponents #-}

-- | `HSI` color model
instance Functor (Pixel HSI) where
  fmap f (PixelHSI h s i) = PixelHSI (f h) (f s) (f i)
  {-# INLINE fmap #-}

-- | `HSI` color model
instance Applicative (Pixel HSI) where
  pure !e = PixelHSI e e e
  {-# INLINE pure #-}
  (PixelHSI fh fs fi) <*> (PixelHSI h s i) = PixelHSI (fh h) (fs s) (fi i)
  {-# INLINE (<*>) #-}

-- | `HSI` color model
instance Foldable (Pixel HSI) where
  foldr f !z (PixelHSI h s i) = f h (f s (f i z))
  {-# INLINE foldr #-}

-- | `HSI` color model
instance Traversable (Pixel HSI) where
  traverse f (PixelHSI h s i) = PixelHSI <$> f h <*> f s <*> f i
  {-# INLINE traverse #-}

-- | `HSI` color model
instance Storable e => Storable (Pixel HSI e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelHSI
  {-# INLINE peek #-}
  poke p (PixelHSI h s i) = poke3 p h s i
  {-# INLINE poke #-}

-- TODO: switch to Either
hsi2rgb :: Pixel HSI Double -> Pixel RGB Double
hsi2rgb (PixelHSI h' s i) = getRGB (h' * 2 * pi)
  where
    !is = i * s
    !second = i - is
    !pi3 = pi / 3
    errorHue = error $ "HSI pixel is not properly scaled, Hue: " ++ show h'
    getFirst !a !b = i + is * cos a / cos b
    {-# INLINE getFirst #-}
    getThird !v1 !v2 = i + 2 * is + v1 - v2
    {-# INLINE getThird #-}
    getRGB h
      | h < 0 = errorHue
      | h < 2 * pi3 =
        let !r = getFirst h (pi3 - h)
            !b = second
            !g = getThird b r
         in PixelRGB r g b
      | h < 4 * pi3 =
        let !g = getFirst (h - 2 * pi3) (h + pi)
            !r = second
            !b = getThird r g
         in PixelRGB r g b
      | h < 2 * pi =
        let !b = getFirst (h - 4 * pi3) (2 * pi - pi3 - h)
            !g = second
            !r = getThird g b
         in PixelRGB r g b
      | otherwise = errorHue
    {-# INLINE getRGB #-}
{-# INLINE hsi2rgb #-}


rgb2hsi :: Pixel RGB Double -> Pixel HSI Double
rgb2hsi (PixelRGB r g b) = PixelHSI h s i
  where
    !h' = atan2 y x
    !h'2pi = h' / (2 * pi)
    !h
      | h' < 0 = h'2pi + 1
      | otherwise = h'2pi
    !s
      | i == 0 = 0
      | otherwise = 1 - minimum [r, g, b] / i
    !i = (r + g + b) / 3
    !x = (2 * r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
{-# INLINE rgb2hsi #-}
