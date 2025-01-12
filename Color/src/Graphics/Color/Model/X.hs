{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Model.X
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.X
  ( X
  -- * Constructors for X color model.
  , pattern ColorX
  , pattern ColorXA
  , Color(X)
  , Weights(..)
  , rgb2y
  ) where

import Data.List.NonEmpty
import Data.Coerce
import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

-------------
--- X ---
-------------

-- | A color with a single channel, most likely luminance
data X

-- | A single channel color `X`
newtype instance Color X e = X e

-- | Constructor for @X@
pattern ColorX :: e -> Color X e
pattern ColorX y = X y
{-# COMPLETE ColorX #-}

-- | Constructor for @X@ with alpha channel.
pattern ColorXA :: e -> e -> Color (Alpha X) e
pattern ColorXA y a = Alpha (X y) a
{-# COMPLETE ColorXA #-}

-- | `X` color model
deriving instance Eq e => Eq (Color X e)
-- | `X` color model
deriving instance Ord e => Ord (Color X e)
-- | `X` color model
deriving instance Storable e => Storable (Color X e)


-- | `X` color model
instance Elevator e => Show (Color X e) where
  showsPrec _ = showsColorModel

-- | `X` color model
instance Elevator e => ColorModel X e where
  type Components X e = e
  type ChannelCount X = 1
  channelCount _ = 1
  {-# INLINE channelCount #-}
  channelNames _ = "Gray" :| []
  channelColors _ = V3 0x80 0x80 0x80 :| []
  toComponents (X y) = y
  {-# INLINE toComponents #-}
  fromComponents = X
  {-# INLINE fromComponents #-}

-- | `X` color model
instance Functor (Color X) where
  fmap f (X y) = X (f y)
  {-# INLINE fmap #-}

-- | `X` color model
instance Applicative (Color X) where
  pure = X
  {-# INLINE pure #-}
  (X fy) <*> (X y) = X (fy y)
  {-# INLINE (<*>) #-}

-- | `X` color model
instance Foldable (Color X) where
  foldr f !z (X y) = f y z
  {-# INLINE foldr #-}

-- | `X` color model
instance Traversable (Color X) where
  traverse f (X y) = X <$> f y
  {-# INLINE traverse #-}

-- | Convert an RGB color model to a single channel by using the supplied weights
--
-- @since 0.1.0
rgb2y ::
     forall e e'. (Elevator e', Elevator e, RealFloat e)
  => Color RGB e'
  -> Weights e
  -> Color X e
rgb2y rgb weights =
  X (coerce (fmap toRealFloat rgb :: Color RGB e) `dotProduct` coerce weights)
{-# INLINE rgb2y #-}

-- | Weights imposed on individual channels of a 3-component color
--
-- @since 0.1.0
newtype Weights e = Weights
  { unWeights :: V3 e
  } deriving (Eq, Num, Show, Fractional, Floating, Functor)
