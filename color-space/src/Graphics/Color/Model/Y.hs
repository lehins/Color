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
-- Module      : Graphics.Color.Model.Y
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.Y
  ( Y
  -- * Constructors for Y color model.
  , pattern PixelY
  , pattern PixelYA
  , Pixel
  , Weights(..)
  , rgb2y
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB
import Graphics.Color.Algebra

-------------
--- Y ---
-------------

-- | Luminance of a color
data Y

-- | Luminance `Y`
newtype instance Pixel Y e = PixelY e

-- | Constructor for @Y@ with alpha channel.
pattern PixelYA :: e -> e -> Pixel (Alpha Y) e
pattern PixelYA y a = Alpha (PixelY y) a
{-# COMPLETE PixelYA #-}

-- | `Y` color model
deriving instance Eq e => Eq (Pixel Y e)
-- | `Y` color model
deriving instance Ord e => Ord (Pixel Y e)
-- | `Y` color model
deriving instance Storable e => Storable (Pixel Y e)


-- | `Y` color model
instance Elevator e => Show (Pixel Y e) where
  showsPrec _ = showsColorModel

-- | `Y` color model
instance Elevator e => ColorModel Y e where
  type Components Y e = e
  toComponents (PixelY y) = y
  {-# INLINE toComponents #-}
  fromComponents = PixelY
  {-# INLINE fromComponents #-}

-- | `Y` color model
instance Functor (Pixel Y) where
  fmap f (PixelY y) = PixelY (f y)
  {-# INLINE fmap #-}

-- | `Y` color model
instance Applicative (Pixel Y) where
  pure = PixelY
  {-# INLINE pure #-}
  (PixelY fy) <*> (PixelY y) = PixelY (fy y)
  {-# INLINE (<*>) #-}

-- | `Y` color model
instance Foldable (Pixel Y) where
  foldr f !z (PixelY y) = f y z
  {-# INLINE foldr #-}

-- | `Y` color model
instance Traversable (Pixel Y) where
  traverse f (PixelY y) = PixelY <$> f y
  {-# INLINE traverse #-}


rgb2y ::
     forall e e'. (Elevator e', Elevator e, RealFloat e)
  => Pixel RGB e'
  -> Weights e
  -> Pixel Y e
rgb2y rgb weights =
  PixelY (coerce (fmap toRealFloat rgb :: Pixel RGB e) `dotProduct` coerce weights)
{-# INLINE rgb2y #-}


newtype Weights e = Weights
  { unWeights :: V3 e
  }
