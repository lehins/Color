{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.YUV.Y
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.YUV.Y
  ( Y
  -- * Constructors for an Y color space.
  , pattern PixelY
  , pattern PixelYA
  , Pixel
  , y2rgb
  , rgb2y
  ) where

import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB

-------------
--- Y ---
-------------

-- | Luma component (commonly denoted as __Y'__)
data Y

-- | `Y` color space
newtype instance Pixel Y e = PixelY e

-- | Constructor for @Y@ with alpha channel.
pattern PixelYA :: e -> e -> Pixel (Alpha Y) e
pattern PixelYA y a = Alpha (PixelY y) a
{-# COMPLETE PixelYA #-}

-- | `Y` color space
deriving instance Eq e => Eq (Pixel Y e)
-- | `Y` color space
deriving instance Ord e => Ord (Pixel Y e)
-- | `Y` color space
deriving instance Storable e => Storable (Pixel Y e)


-- | `Y` color space
instance Elevator e => Show (Pixel Y e) where
  showsPrec _ = showsColorModel

-- | `Y` color space
instance Elevator e => ColorModel Y e where
  type Components Y e = e
  toComponents (PixelY y) = y
  {-# INLINE toComponents #-}
  fromComponents = PixelY
  {-# INLINE fromComponents #-}

-- | `Y` color space
instance Functor (Pixel Y) where
  fmap f (PixelY y) = PixelY (f y)
  {-# INLINE fmap #-}

-- | `Y` color space
instance Applicative (Pixel Y) where
  pure = PixelY
  {-# INLINE pure #-}
  (PixelY fy) <*> (PixelY y) = PixelY (fy y)
  {-# INLINE (<*>) #-}

-- | `Y` color space
instance Foldable (Pixel Y) where
  foldr f !z (PixelY y) = f y z
  {-# INLINE foldr #-}

-- | `Y` color space
instance Traversable (Pixel Y) where
  traverse f (PixelY y) = PixelY <$> f y
  {-# INLINE traverse #-}


y2rgb :: Pixel Y Double -> Pixel RGB Double
y2rgb (PixelY y) = PixelRGB y y y
{-# INLINE y2rgb #-}

rgb2y :: Pixel RGB Double -> Pixel Y Double
rgb2y (PixelRGB r g b) = PixelY $ 0.299 * r + 0.587 * g + 0.114 * b
{-# INLINE rgb2y #-}
