{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.YUV.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.YUV.YCbCr
  ( YCbCr
  -- * Constructors for an YCbCr color space.
  , pattern PixelYCbCr
  , pattern PixelYCbCrA
  , ToYCbCr(..)
  , Pixel
  ) where

import Foreign.Storable
import Graphics.ColorSpace.Algebra
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal

-------------
--- YCbCr ---
-------------

-- | Luminance (denoted as Y), Chrominance toward Blue, and Chrominance toward Red (color
-- component values)
data YCbCr

-- | `YCbCr` color space
newtype instance Pixel YCbCr e = YCbCr (V3 e)


pattern PixelYCbCr :: e -> e -> e -> Pixel YCbCr e
pattern PixelYCbCr y cb cr = YCbCr (V3 y cb cr)
{-# COMPLETE PixelYCbCr #-}

-- | Constructor for @YCbCr@ with alpha channel.
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha YCbCr) e
pattern PixelYCbCrA y cb cr a = Alpha (YCbCr (V3 y cb cr)) a
{-# COMPLETE PixelYCbCrA #-}

-- | `YCbCr` color space
deriving instance Eq e => Eq (Pixel YCbCr e)
-- | `YCbCr` color space
deriving instance Ord e => Ord (Pixel YCbCr e)

-- | `YCbCr` color space
instance Elevator e => Show (Pixel YCbCr e) where
  showsPrec _ = showsColorModel

-- | `YCbCr` color space
instance Elevator e => ColorModel YCbCr e where
  type Components YCbCr e = (e, e, e)
  toComponents (PixelYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = PixelYCbCr y cb cr
  {-# INLINE fromComponents #-}

-- | `YCbCr` color space
deriving instance Functor (Pixel YCbCr)

-- | `YCbCr` color space
deriving instance Applicative (Pixel YCbCr)

-- | `YCbCr` color space
deriving instance Foldable (Pixel YCbCr)

-- | `YCbCr` color space
deriving instance Traversable (Pixel YCbCr)

-- | `YCbCr` color space
deriving instance Storable e => Storable (Pixel YCbCr e)



-- | Conversion to `YCbCr` color space.
class ToYCbCr cs where
  -- | Convert to an `YCbCr` pixel.
  toPixelYCbCr :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel YCbCr a
  -- | Convert to an `YCbCr` pixel with alpha channel
  toPixelYCbCrA :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel (Alpha YCbCr) a
  toPixelYCbCrA = (`addAlpha` 1) . toPixelYCbCr
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr YCbCr where
  toPixelYCbCr = fmap toRealFloat
  {-# INLINE toPixelYCbCr #-}
