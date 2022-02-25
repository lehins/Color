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
-- Module      : Graphics.Color.Model.HSL
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.HSL
  ( HSL
  -- * Constructors for an HSL color model.
  , pattern ColorHSL
  , pattern ColorHSLA
  , pattern ColorH360SL
  , Color
  , ColorModel(..)
  , hc2rgb
  , hsl2rgb
  , rgb2hsl
  ) where

import Foreign.Storable
import Graphics.Color.Model.HSV (hc2rgb)
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

-----------
--- HSL ---
-----------

-- | Hue, Saturation and Luminance (Lightness) color model.
data HSL

-- | `HSL` color model
newtype instance Color HSL e = HSL (V3 e)

-- | Constructor for @HSL@.
pattern ColorHSL :: e -> e -> e -> Color HSL e
pattern ColorHSL h s l = HSL (V3 h s l)
{-# COMPLETE ColorHSL #-}


-- | Constructor for @HSL@ with alpha channel.
pattern ColorHSLA :: e -> e -> e -> e -> Color (Alpha HSL) e
pattern ColorHSLA h s l a = Alpha (ColorHSL h s l) a
{-# COMPLETE ColorHSLA #-}

-- | Constructor for an HSL color model. Difference from `ColorHSL` is that channels are
-- restricted to `Double` and the hue is specified in 0 to 360 degree range, rather than 0
-- to 1. Note, that this is not checked.
pattern ColorH360SL :: Fractional e => e -> e -> e -> Color HSL e
pattern ColorH360SL h s l <- ColorHSL ((* 360) -> h) s l where
        ColorH360SL h s l = ColorHSL (h / 360) s l
{-# COMPLETE ColorH360SL #-}

-- | `HSL` color model
deriving instance Eq e => Eq (Color HSL e)
-- | `HSL` color model
deriving instance Ord e => Ord (Color HSL e)
-- | `HSL` color model
deriving instance Functor (Color HSL)
-- | `HSL` color model
deriving instance Applicative (Color HSL)
-- | `HSL` color model
deriving instance Foldable (Color HSL)
-- | `HSL` color model
deriving instance Traversable (Color HSL)
-- | `HSL` color model
deriving instance Storable e => Storable (Color HSL e)

-- | `HSL` color model
instance Elevator e => Show (Color HSL e) where
  showsPrec _ = showsColorModel

-- | `HSL` color model
instance Elevator e => ColorModel HSL e where
  type Components HSL e = (e, e, e)
  toComponents (ColorHSL h s l) = (h, s, l)
  {-# INLINE toComponents #-}
  fromComponents (h, s, l) = ColorHSL h s l
  {-# INLINE fromComponents #-}

hsl2rgb :: RealFrac e => Color HSL e -> Color RGB e
hsl2rgb (ColorHSL h s l) = (+ m) <$> hc2rgb h c
  where
    !c = (1 - abs (2 * l - 1)) * s
    !m = l - c / 2
{-# INLINE hsl2rgb #-}


rgb2hsl :: (Ord e, Floating e) => Color RGB e -> Color HSL e
rgb2hsl (ColorRGB r g b) = ColorHSL h s l
  where
    !max' = max r (max g b)
    !min' = min r (min g b)
    !c' = max' - min'
    !h' | c'   == 0 = 0
        | max' == r = (    (g - b) / c') / 6
        | max' == g = (2 + (b - r) / c') / 6
        | max' == b = (4 + (r - g) / c') / 6
        | otherwise = 0
    !h
      | h' < 0 = h' + 1
      | otherwise = h'
    !s
      | max' == 0 || min' == 1 = 0
      | otherwise = (max' - l) / min l (1 - l)
    !l = (max' + min') / 2
{-# INLINE rgb2hsl #-}
