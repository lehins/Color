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
  , pattern ColorY
  , pattern ColorYA
  , Color
  , Weights(..)
  , rgb2y
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

-------------
--- Y ---
-------------

-- | Luminance of a color
data Y

-- | Luminance `Y`
newtype instance Color Y e = ColorY e

-- | Constructor for @Y@ with alpha channel.
pattern ColorYA :: e -> e -> Color (Alpha Y) e
pattern ColorYA y a = Alpha (ColorY y) a
{-# COMPLETE ColorYA #-}

-- | `Y` color model
deriving instance Eq e => Eq (Color Y e)
-- | `Y` color model
deriving instance Ord e => Ord (Color Y e)
-- | `Y` color model
deriving instance Storable e => Storable (Color Y e)


-- | `Y` color model
instance Elevator e => Show (Color Y e) where
  showsPrec _ = showsColorModel

-- | `Y` color model
instance Elevator e => ColorModel Y e where
  type Components Y e = e
  toComponents (ColorY y) = y
  {-# INLINE toComponents #-}
  fromComponents = ColorY
  {-# INLINE fromComponents #-}

-- | `Y` color model
instance Functor (Color Y) where
  fmap f (ColorY y) = ColorY (f y)
  {-# INLINE fmap #-}

-- | `Y` color model
instance Applicative (Color Y) where
  pure = ColorY
  {-# INLINE pure #-}
  (ColorY fy) <*> (ColorY y) = ColorY (fy y)
  {-# INLINE (<*>) #-}

-- | `Y` color model
instance Foldable (Color Y) where
  foldr f !z (ColorY y) = f y z
  {-# INLINE foldr #-}

-- | `Y` color model
instance Traversable (Color Y) where
  traverse f (ColorY y) = ColorY <$> f y
  {-# INLINE traverse #-}


rgb2y ::
     forall e e'. (Elevator e', Elevator e, RealFloat e)
  => Color RGB e'
  -> Weights e
  -> Color Y e
rgb2y rgb weights =
  ColorY (coerce (fmap toRealFloat rgb :: Color RGB e) `dotProduct` coerce weights)
{-# INLINE rgb2y #-}


newtype Weights e = Weights
  { unWeights :: V3 e
  }
