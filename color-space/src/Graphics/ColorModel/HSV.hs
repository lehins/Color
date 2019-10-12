{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.HSV
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.HSV
  ( HSV
  , Pixel(..)
  , hsv2rgb
  , rgb2hsv
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB

-----------
--- HSV ---
-----------

-- | Hue, Saturation and Intensity color space.
data HSV

-- | `HSV` color model
data instance Pixel HSV e = PixelHSV !e !e !e
-- | `HSV` color model
deriving instance Eq e => Eq (Pixel HSV e)
-- | `HSV` color model
deriving instance Ord e => Ord (Pixel HSV e)

-- | `HSV` color model
instance Elevator e => Show (Pixel HSV e) where
  showsPrec _ = showsColorModel

-- | `HSV` color model
instance Elevator e => ColorModel HSV e where
  type Components HSV e = (e, e, e)
  toComponents (PixelHSV h s v) = (h, s, v)
  {-# INLINE toComponents #-}
  fromComponents (h, s, v) = PixelHSV h s v
  {-# INLINE fromComponents #-}

-- | `HSV` color model
instance Functor (Pixel HSV) where
  fmap f (PixelHSV h s v) = PixelHSV (f h) (f s) (f v)
  {-# INLINE fmap #-}

-- | `HSV` color model
instance Applicative (Pixel HSV) where
  pure !e = PixelHSV e e e
  {-# INLINE pure #-}
  (PixelHSV fh fs fv) <*> (PixelHSV h s v) = PixelHSV (fh h) (fs s) (fv v)
  {-# INLINE (<*>) #-}

-- | `HSV` color model
instance Foldable (Pixel HSV) where
  foldr f !z (PixelHSV h s v) = f h (f s (f v z))
  {-# INLINE foldr #-}

-- | `HSV` color model
instance Traversable (Pixel HSV) where
  traverse f (PixelHSV h s v) = PixelHSV <$> f h <*> f s <*> f v
  {-# INLINE traverse #-}

-- | `HSV` color model
instance Storable e => Storable (Pixel HSV e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelHSV
  {-# INLINE peek #-}
  poke p (PixelHSV h s v) = poke3 p h s v
  {-# INLINE poke #-}

hc2rgb :: Double -> Double -> Pixel RGB Double
hc2rgb h c
  | h' < 0 = PixelRGB 0 0 0
  | h' <= 1 = PixelRGB c x 0
  | h' <= 2 = PixelRGB x c 0
  | h' <= 3 = PixelRGB 0 c x
  | h' <= 4 = PixelRGB 0 x c
  | h' <= 5 = PixelRGB x 0 c
  | h' <= 6 = PixelRGB c 0 x
  | otherwise = PixelRGB 0 0 0
  where
    !h' = h * 6
    !hTrunc = truncate h' :: Int
    !hMod2 = fromIntegral (hTrunc `mod` 2) + (h' - fromIntegral hTrunc)
    !x = c * (1 - abs (hMod2 - 1))

-- TODO: switch to Either
hsv2rgb :: Pixel HSV Double -> Pixel RGB Double
hsv2rgb (PixelHSV h s v) = (+ m) <$> hc2rgb h c
  where
    !c = v * s
    !m = v - c


rgb2hsv :: Pixel RGB Double -> Pixel HSV Double
rgb2hsv (PixelRGB r g b) = PixelHSV h s v
  where
    !max' = max r (max g b)
    !min' = min r (min g b)
    h' | max' == r = (    (g - b) / (max' - min')) / 6
       | max' == g = (2 + (b - r) / (max' - min')) / 6
       | max' == b = (4 + (r - g) / (max' - min')) / 6
       | otherwise = 0
    !h
      | h' < 0 = h' + 1
      | otherwise = h'
    !s
      | max' == 0 = 0
      | otherwise = (max' - min') / max'
    !v = max'
{-# INLINE rgb2hsv #-}
