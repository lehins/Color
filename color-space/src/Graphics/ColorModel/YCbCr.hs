{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.YCbCr
  ( YCbCr
  , Pixel(..)
  , ycbcr2rgb
  , rgb2ycbcr
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB

-------------
--- YCbCr ---
-------------

-- | Luma component (commonly denoted as __Y'__), blue and red difference chroma components
data YCbCr

-- | `YCbCr` color model
data instance Pixel YCbCr e = PixelYCbCr !e !e !e
-- | `YCbCr` color model
deriving instance Eq e => Eq (Pixel YCbCr e)
-- | `YCbCr` color model
deriving instance Ord e => Ord (Pixel YCbCr e)

-- | `YCbCr` color model
instance Elevator e => Show (Pixel YCbCr e) where
  showsPrec _ = showsColorModel

-- | `YCbCr` color model
instance Elevator e => ColorModel YCbCr e where
  type Components YCbCr e = (e, e, e)
  toComponents (PixelYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = PixelYCbCr y cb cr
  {-# INLINE fromComponents #-}

-- | `YCbCr` color model
instance Functor (Pixel YCbCr) where
  fmap f (PixelYCbCr y cb cr) = PixelYCbCr (f y) (f cb) (f cr)
  {-# INLINE fmap #-}

-- | `YCbCr` color model
instance Applicative (Pixel YCbCr) where
  pure !e = PixelYCbCr e e e
  {-# INLINE pure #-}
  (PixelYCbCr fh fs fi) <*> (PixelYCbCr y cb cr) = PixelYCbCr (fh y) (fs cb) (fi cr)
  {-# INLINE (<*>) #-}

-- | `YCbCr` color model
instance Foldable (Pixel YCbCr) where
  foldr f !z (PixelYCbCr y cb cr) = f y (f cb (f cr z))
  {-# INLINE foldr #-}

-- | `YCbCr` color model
instance Traversable (Pixel YCbCr) where
  traverse f (PixelYCbCr y cb cr) = PixelYCbCr <$> f y <*> f cb <*> f cr
  {-# INLINE traverse #-}

-- | `YCbCr` color model
instance Storable e => Storable (Pixel YCbCr e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelYCbCr
  {-# INLINE peek #-}
  poke p (PixelYCbCr y cb cr) = poke3 p y cb cr
  {-# INLINE poke #-}

ycbcr2rgb :: Pixel YCbCr Double -> Pixel RGB Double
ycbcr2rgb (PixelYCbCr y cb cr) = PixelRGB r g b
  where
    !cb05 = cb - 0.5
    !cr05 = cr - 0.5
    !r = clamp01 (y                  +   1.402 * cr05)
    !g = clamp01 (y - 0.34414 * cb05 - 0.71414 * cr05)
    !b = clamp01 (y +   1.772 * cb05)

rgb2ycbcr :: Pixel RGB Double -> Pixel YCbCr Double
rgb2ycbcr (PixelRGB r g b) = PixelYCbCr y cb cr
  where
    !y  =          0.299 * r +    0.587 * g +    0.114 * b
    !cb = 0.5 - 0.168736 * r - 0.331264 * g +      0.5 * b
    !cr = 0.5 +      0.5 * r - 0.418688 * g - 0.081312 * b
{-# INLINE rgb2ycbcr #-}
