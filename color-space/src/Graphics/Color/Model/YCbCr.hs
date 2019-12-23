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
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.YCbCr
  ( pattern PixelYCbCr
  , pattern PixelYCbCrA
  , YCbCr
  , Pixel(YCbCr)
  , rgb2ycbcr
  , ycbcr2rgb
  , module Graphics.Color.Model
  ) where

import Foreign.Storable
import Graphics.Color.Model
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB
import Graphics.Color.Model.Y
import Graphics.Color.Algebra

-- | `YCbCr` color model
data YCbCr

-- | `YCbCr` color model
newtype instance Pixel YCbCr e = YCbCr (V3 e)

-- | `YCbCr` color model
deriving instance Eq e => Eq (Pixel YCbCr e)
-- | `YCbCr` color model
deriving instance Ord e => Ord (Pixel YCbCr e)
-- | `YCbCr` color model
deriving instance Functor (Pixel YCbCr)
-- | `YCbCr` color model
deriving instance Applicative (Pixel YCbCr)
-- | `YCbCr` color model
deriving instance Foldable (Pixel YCbCr)
-- | `YCbCr` color model
deriving instance Traversable (Pixel YCbCr)
-- | `YCbCr` color model
deriving instance Storable e => Storable (Pixel YCbCr e)

-- | `YCbCr` color model
instance Elevator e => Show (Pixel YCbCr e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color model in an alternative YCbCr color model
pattern PixelYCbCr :: e -> e -> e -> Pixel YCbCr e
pattern PixelYCbCr y cb cr = YCbCr (V3 y cb cr)
{-# COMPLETE PixelYCbCr #-}

-- | Constructor for @YCbCr@ with alpha channel.
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha YCbCr) e
pattern PixelYCbCrA y cb cr a = Alpha (YCbCr (V3 y cb cr)) a
{-# COMPLETE PixelYCbCrA #-}


-- | `YCbCr` color model
instance Elevator e => ColorModel YCbCr e where
  type Components YCbCr e = (e, e, e)
  toComponents (PixelYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = PixelYCbCr y cb cr
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("YCbCr" ++)


rgb2ycbcr :: (Elevator e', Elevator e, RealFloat e) => Pixel RGB e' -> Weights e -> Pixel YCbCr e
rgb2ycbcr rgb' weights@(Weights (V3 kr _ kb)) = PixelYCbCr y cb cr
  where
    rgb@(PixelRGB r _ b) = toRealFloat <$> rgb'
    PixelY y = rgb2y rgb weights
    !cb = 0.5 + 0.5 * (b - y) / (1 - kb)
    !cr = 0.5 + 0.5 * (r - y) / (1 - kr)
{-# INLINE rgb2ycbcr #-}


ycbcr2rgb :: (Elevator e', Elevator e, RealFloat e) => Pixel YCbCr e' -> Weights e -> Pixel RGB e
ycbcr2rgb ycbcr (Weights (V3 kr kg kb)) = PixelRGB r g b
  where
    PixelYCbCr y cb cr = toRealFloat <$> ycbcr
    !r = y + (2 - 2 * kr) * cr
    !b = y + (2 - 2 * kb) * cb
    !g = (y - kr * r - kb * b) / kg
{-# INLINE ycbcr2rgb #-}
