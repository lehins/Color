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
  , pattern ColorHSV
  , pattern ColorHSVA
  , pattern ColorH360SV
  , Color
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
data instance Color HSV e = ColorHSV !e !e !e

-- | Constructor for @HSV@ with alpha channel.
pattern ColorHSVA :: e -> e -> e -> e -> Color (Alpha HSV) e
pattern ColorHSVA h s v a = Alpha (ColorHSV h s v) a
{-# COMPLETE ColorHSVA #-}

-- | Constructor for an HSV color model. Difference from `ColorHSV` is that channels are
-- restricted to `Double` and the hue is specified in 0 to 360 degree range, rather than 0
-- to 1. Note, that this is not checked.
pattern ColorH360SV :: Double -> Double -> Double -> Color HSV Double
pattern ColorH360SV h s v <- ColorHSV ((* 360) -> h) s v where
        ColorH360SV h s v = ColorHSV (h / 360) s v
{-# COMPLETE ColorH360SV #-}

-- | `HSV` color model
deriving instance Eq e => Eq (Color HSV e)
-- | `HSV` color model
deriving instance Ord e => Ord (Color HSV e)

-- | `HSV` color model
instance Elevator e => Show (Color HSV e) where
  showsPrec _ = showsColorModel

-- | `HSV` color model
instance Elevator e => ColorModel HSV e where
  type Components HSV e = (e, e, e)
  toComponents (ColorHSV h s v) = (h, s, v)
  {-# INLINE toComponents #-}
  fromComponents (h, s, v) = ColorHSV h s v
  {-# INLINE fromComponents #-}

-- | `HSV` color model
instance Functor (Color HSV) where
  fmap f (ColorHSV h s v) = ColorHSV (f h) (f s) (f v)
  {-# INLINE fmap #-}

-- | `HSV` color model
instance Applicative (Color HSV) where
  pure !e = ColorHSV e e e
  {-# INLINE pure #-}
  (ColorHSV fh fs fv) <*> (ColorHSV h s v) = ColorHSV (fh h) (fs s) (fv v)
  {-# INLINE (<*>) #-}

-- | `HSV` color model
instance Foldable (Color HSV) where
  foldr f !z (ColorHSV h s v) = f h (f s (f v z))
  {-# INLINE foldr #-}

-- | `HSV` color model
instance Traversable (Color HSV) where
  traverse f (ColorHSV h s v) = ColorHSV <$> f h <*> f s <*> f v
  {-# INLINE traverse #-}

-- | `HSV` color model
instance Storable e => Storable (Color HSV e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 ColorHSV
  {-# INLINE peek #-}
  poke p (ColorHSV h s v) = poke3 p h s v
  {-# INLINE poke #-}

hc2rgb :: RealFrac e => e -> e -> Color RGB e
hc2rgb h c
  | h' < 0 = ColorRGB 0 0 0
  | h' <= 1 = ColorRGB c x 0
  | h' <= 2 = ColorRGB x c 0
  | h' <= 3 = ColorRGB 0 c x
  | h' <= 4 = ColorRGB 0 x c
  | h' <= 5 = ColorRGB x 0 c
  | h' <= 6 = ColorRGB c 0 x
  | otherwise = ColorRGB 0 0 0
  where
    !h' = h * 6
    !hTrunc = truncate h' :: Int
    !hMod2 = fromIntegral (hTrunc `mod` 2) + (h' - fromIntegral hTrunc)
    !x = c * (1 - abs (hMod2 - 1))
{-# INLINE hc2rgb #-}

hsv2rgb :: RealFrac e => Color HSV e -> Color RGB e
hsv2rgb (ColorHSV h s v) = (+ m) <$> hc2rgb h c
  where
    !c = v * s
    !m = v - c
{-# INLINE hsv2rgb #-}


rgb2hsv :: (Ord e, Fractional e) => Color RGB e -> Color HSV e
rgb2hsv (ColorRGB r g b) = ColorHSV h s v
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
