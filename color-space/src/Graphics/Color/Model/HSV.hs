{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Model.HSV
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.HSV
  ( HSV
  -- * Constructors for an HSV color model.
  , pattern PixelHSV
  , pattern PixelHSVA
  , pattern PixelH360SV
  , Pixel
  , ColorModel(..)
  , hc2rgb
  , hsv2rgb
  , rgb2hsv
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

-----------
--- HSV ---
-----------

-- | Hue, Saturation and Value (Brightness) color model.
data HSV

-- | `HSV` color model
data instance Pixel HSV e = PixelHSV !e !e !e

-- | Constructor for @HSV@ with alpha channel.
pattern PixelHSVA :: e -> e -> e -> e -> Pixel (Alpha HSV) e
pattern PixelHSVA h s v a = Alpha (PixelHSV h s v) a
{-# COMPLETE PixelHSVA #-}

-- | Constructor for an HSV color model. Difference from `PixelHSV` is that channels are
-- restricted to `Double` and the hue is specified in 0 to 360 degree range, rather than 0
-- to 1. Note, that this is not checked.
pattern PixelH360SV :: Double -> Double -> Double -> Pixel HSV Double
pattern PixelH360SV h s v <- PixelHSV ((* 360) -> h) s v where
        PixelH360SV h s v = PixelHSV (h / 360) s v
{-# COMPLETE PixelH360SV #-}

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

hc2rgb :: RealFrac e => e -> e -> Pixel RGB e
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
{-# INLINE hc2rgb #-}

hsv2rgb :: RealFrac e => Pixel HSV e -> Pixel RGB e
hsv2rgb (PixelHSV h s v) = (+ m) <$> hc2rgb h c
  where
    !c = v * s
    !m = v - c
{-# INLINE hsv2rgb #-}


rgb2hsv :: (Ord e, Fractional e) => Pixel RGB e -> Pixel HSV e
rgb2hsv (PixelRGB r g b) = PixelHSV h s v
  where
    !max' = max r (max g b)
    !min' = min r (min g b)
    !h' | max' == r = (    (g - b) / (max' - min')) / 6
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
