{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Color.Model.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.YCbCr
  ( pattern ColorYCbCr
  , pattern ColorYCbCrA
  , YCbCr
  , Color(YCbCr)
  , rgb2ycbcr
  , ycbcr2rgb
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB
import Graphics.Color.Model.X

-- | `YCbCr` color model
data YCbCr

-- | `YCbCr` color model
newtype instance Color YCbCr e = YCbCr (V3 e)

-- | `YCbCr` color model
deriving instance Eq e => Eq (Color YCbCr e)
-- | `YCbCr` color model
deriving instance Ord e => Ord (Color YCbCr e)
-- | `YCbCr` color model
deriving instance Functor (Color YCbCr)
-- | `YCbCr` color model
deriving instance Applicative (Color YCbCr)
-- | `YCbCr` color model
deriving instance Foldable (Color YCbCr)
-- | `YCbCr` color model
deriving instance Traversable (Color YCbCr)
-- | `YCbCr` color model
deriving instance Storable e => Storable (Color YCbCr e)

-- | `YCbCr` color model
instance Elevator e => Show (Color YCbCr e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color model in an alternative YCbCr color model
pattern ColorYCbCr :: e -> e -> e -> Color YCbCr e
pattern ColorYCbCr y cb cr = YCbCr (V3 y cb cr)
{-# COMPLETE ColorYCbCr #-}

-- | Constructor for @YCbCr@ with alpha channel.
pattern ColorYCbCrA :: e -> e -> e -> e -> Color (Alpha YCbCr) e
pattern ColorYCbCrA y cb cr a = Alpha (YCbCr (V3 y cb cr)) a
{-# COMPLETE ColorYCbCrA #-}


-- | `YCbCr` color model
instance Elevator e => ColorModel YCbCr e where
  type Components YCbCr e = (e, e, e)
  toComponents (ColorYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = ColorYCbCr y cb cr
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("YCbCr" ++)


rgb2ycbcr :: (Elevator e', Elevator e, RealFloat e) => Color RGB e' -> Weights e -> Color YCbCr e
rgb2ycbcr rgb' weights@(Weights (V3 kr _ kb)) = ColorYCbCr y' cb cr
  where
    rgb@(ColorRGB r' _ b') = toRealFloat <$> rgb'
    ColorX y' = rgb2y rgb weights
    !cb = 0.5 + 0.5 * (b' - y') / (1 - kb)
    !cr = 0.5 + 0.5 * (r' - y') / (1 - kr)
{-# INLINE rgb2ycbcr #-}


ycbcr2rgb :: (Elevator e', Elevator e, RealFloat e) => Color YCbCr e' -> Weights e -> Color RGB e
ycbcr2rgb ycbcr (Weights (V3 kr kg kb)) = ColorRGB r' g' b'
  where
    ColorYCbCr y' cb cr = toRealFloat <$> ycbcr
    !r' = clamp01 (y' + (2 - 2 * kr) * (cr - 0.5))
    !b' = clamp01 (y' + (2 - 2 * kb) * (cb - 0.5))
    !g' = clamp01 ((y' - kr * r' - kb * b') / kg)
{-# INLINE ycbcr2rgb #-}
