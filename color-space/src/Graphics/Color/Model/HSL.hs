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
-- Module      : Graphics.Color.Model.HSL
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.HSL
  ( HSL
  -- * Constructors for an HSL color model.
  , pattern PixelHSL
  , pattern PixelHSLA
  , pattern PixelH360SL
  , Pixel
  , ColorModel(..)
  , hc2rgb
  , hsl2rgb
  , rgb2hsl
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB
import Graphics.Color.Model.HSV (hc2rgb)

-----------
--- HSL ---
-----------

-- | Hue, Saturation and Luminance (Lightness) color model.
data HSL

-- | `HSL` color model
data instance Pixel HSL e = PixelHSL !e !e !e

-- | Constructor for @HSL@ with alpha channel.
pattern PixelHSLA :: e -> e -> e -> e -> Pixel (Alpha HSL) e
pattern PixelHSLA h s l a = Alpha (PixelHSL h s l) a
{-# COMPLETE PixelHSLA #-}

-- | Constructor for an HSL color model. Difference from `PixelHSL` is that channels are
-- restricted to `Double` and the hue is specified in 0 to 360 degree range, rather than 0
-- to 1. Note, that this is not checked.
pattern PixelH360SL :: Double -> Double -> Double -> Pixel HSL Double
pattern PixelH360SL h s l <- PixelHSL ((* 360) -> h) s l where
        PixelH360SL h s l = PixelHSL (h / 360) s l
{-# COMPLETE PixelH360SL #-}

-- | `HSL` color model
deriving instance Eq e => Eq (Pixel HSL e)
-- | `HSL` color model
deriving instance Ord e => Ord (Pixel HSL e)

-- | `HSL` color model
instance Elevator e => Show (Pixel HSL e) where
  showsPrec _ = showsColorModel

-- | `HSL` color model
instance Elevator e => ColorModel HSL e where
  type Components HSL e = (e, e, e)
  toComponents (PixelHSL h s l) = (h, s, l)
  {-# INLINE toComponents #-}
  fromComponents (h, s, l) = PixelHSL h s l
  {-# INLINE fromComponents #-}

-- | `HSL` color model
instance Functor (Pixel HSL) where
  fmap f (PixelHSL h s l) = PixelHSL (f h) (f s) (f l)
  {-# INLINE fmap #-}

-- | `HSL` color model
instance Applicative (Pixel HSL) where
  pure !e = PixelHSL e e e
  {-# INLINE pure #-}
  (PixelHSL fh fs fv) <*> (PixelHSL h s l) = PixelHSL (fh h) (fs s) (fv l)
  {-# INLINE (<*>) #-}

-- | `HSL` color model
instance Foldable (Pixel HSL) where
  foldr f !z (PixelHSL h s l) = f h (f s (f l z))
  {-# INLINE foldr #-}

-- | `HSL` color model
instance Traversable (Pixel HSL) where
  traverse f (PixelHSL h s l) = PixelHSL <$> f h <*> f s <*> f l
  {-# INLINE traverse #-}

-- | `HSL` color model
instance Storable e => Storable (Pixel HSL e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelHSL
  {-# INLINE peek #-}
  poke p (PixelHSL h s l) = poke3 p h s l
  {-# INLINE poke #-}

hsl2rgb :: RealFrac e => Pixel HSL e -> Pixel RGB e
hsl2rgb (PixelHSL h s l) = (+ m) <$> hc2rgb h c
  where
    !c = (1 - abs (2 * l - 1)) * s
    !m = l - c / 2
{-# INLINE hsl2rgb #-}


rgb2hsl :: (Ord e, Floating e) => Pixel RGB e -> Pixel HSL e
rgb2hsl (PixelRGB r g b) = PixelHSL h s l
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
      | max' == 0 || min' == 1 = 0
      | otherwise = (max' - l) / min l (1 - l)
    !l = (max' + min') / 2
{-# INLINE rgb2hsl #-}
