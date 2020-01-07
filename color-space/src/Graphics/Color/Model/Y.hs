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
  , Color(Y)
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
newtype instance Color Y e = Y e

-- | Constructor for @Y@
pattern ColorY :: e -> Color Y e
pattern ColorY y = Y y
{-# COMPLETE ColorY #-}

-- | Constructor for @Y@ with alpha channel.
pattern ColorYA :: e -> e -> Color (Alpha Y) e
pattern ColorYA y a = Alpha (Y y) a
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
  toComponents (Y y) = y
  {-# INLINE toComponents #-}
  fromComponents = Y
  {-# INLINE fromComponents #-}

-- | `Y` color model
instance Functor (Color Y) where
  fmap f (Y y) = Y (f y)
  {-# INLINE fmap #-}

-- | `Y` color model
instance Applicative (Color Y) where
  pure = Y
  {-# INLINE pure #-}
  (Y fy) <*> (Y y) = Y (fy y)
  {-# INLINE (<*>) #-}

-- | `Y` color model
instance Foldable (Color Y) where
  foldr f !z (Y y) = f y z
  {-# INLINE foldr #-}

-- | `Y` color model
instance Traversable (Color Y) where
  traverse f (Y y) = Y <$> f y
  {-# INLINE traverse #-}


rgb2y ::
     forall e e'. (Elevator e', Elevator e, RealFloat e)
  => Color RGB e'
  -> Weights e
  -> Color Y e
rgb2y rgb weights =
  Y (coerce (fmap toRealFloat rgb :: Color RGB e) `dotProduct` coerce weights)
{-# INLINE rgb2y #-}


newtype Weights e = Weights
  { unWeights :: V3 e
  }
