{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Pixel.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Pixel.Internal
  ( Pixel(..)
  , liftPixel
  , toPixel8
  , toPixel16
  , toPixel32
  , toPixel64
  , toPixelF
  , toPixelD
  ) where

import Data.Coerce
import Graphics.Color.Model.Internal
import Foreign.Storable

-- | Digital imaging is one of the most common places for a color to be used in. The
-- smallest element in any image is a pixel, which is defined by its color.
--
-- @since 0.1.0
newtype Pixel cs e = Pixel
  { pixelColor :: Color cs e
  }

deriving instance Eq (Color cs e) => Eq (Pixel cs e)
deriving instance Ord (Color cs e) => Ord (Pixel cs e)
deriving instance Num (Color cs e) => Num (Pixel cs e)
deriving instance Floating (Color cs e) => Floating (Pixel cs e)
deriving instance Fractional (Color cs e) => Fractional (Pixel cs e)
deriving instance Functor (Color cs) => Functor (Pixel cs)
deriving instance Applicative (Color cs) => Applicative (Pixel cs)
deriving instance Foldable (Color cs) => Foldable (Pixel cs)
deriving instance Traversable (Color cs) => Traversable (Pixel cs)
deriving instance Storable (Color cs e) => Storable (Pixel cs e)
instance Show (Color cs e) => Show (Pixel cs e) where
  show = show . pixelColor


-- | Apply a function to `Pixel`'s `Color`
--
-- @since 0.1.0
liftPixel :: (Color cs e -> Color cs' e') -> Pixel cs e -> Pixel cs' e'
liftPixel f = coerce . f . coerce
{-# INLINE liftPixel #-}



-- Elevation

-- | Convert all channels of a pixel to 8bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel8 :: ColorModel cs e => Pixel cs e -> Pixel cs Word8
toPixel8 = liftPixel (fmap toWord8)
{-# INLINE toPixel8 #-}

-- | Convert all channels of a pixel to 16bits each, while appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel16 :: ColorModel cs e => Pixel cs e -> Pixel cs Word16
toPixel16 = liftPixel (fmap toWord16)
{-# INLINE toPixel16 #-}


-- | Convert all channels of a pixel to 32bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel32 :: ColorModel cs e => Pixel cs e -> Pixel cs Word32
toPixel32 = liftPixel (fmap toWord32)
{-# INLINE toPixel32 #-}


-- | Convert all channels of a pixel to 64bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel64 :: ColorModel cs e => Pixel cs e -> Pixel cs Word64
toPixel64 = liftPixel (fmap toWord64)
{-# INLINE toPixel64 #-}


-- | Convert all channels of a pixel to 32bit floating point numers each, while doing
-- appropriate scaling. See `Elevator`.
--
-- @since 0.1.0
toPixelF :: ColorModel cs e => Pixel cs e -> Pixel cs Float
toPixelF = liftPixel (fmap toFloat)
{-# INLINE toPixelF #-}

-- | Convert all channels of a pixel to 64bit floating point numers each, while doing
-- appropriate scaling. See `Elevator`.
--
-- @since 0.1.0
toPixelD :: ColorModel cs e => Pixel cs e -> Pixel cs Double
toPixelD = liftPixel (fmap toDouble)
{-# INLINE toPixelD #-}
