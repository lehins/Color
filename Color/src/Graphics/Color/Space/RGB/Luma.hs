{-# LANGUAGE DeriveFunctor #-}
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
-- Module      : Graphics.Color.Space.RGB.Luma
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Luma
  ( Y'
  -- * Constructors for Luma.
  , pattern Y'
  , Color
  , Luma(..)
  , Weight(..)
  , Weights(..)
  , rgbLuma
  , rgbLumaWeights
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.RGB as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Model.Y
import Graphics.Color.Space.RGB.Internal

-------------
--- Luma ----
-------------

-- | [Luma](https://en.wikipedia.org/wiki/Luma_(video\)) of a non-linear gamma corrected
-- RGB color space.
data Y'

-- | Constructor for Luma.
newtype instance Color Y' e = Y' e

-- | `Y'` color model
deriving instance Eq e => Eq (Color Y' e)
-- | `Y'` color model
deriving instance Ord e => Ord (Color Y' e)
-- | `Y'` color model
deriving instance Storable e => Storable (Color Y' e)


-- | `Y'` color model
instance Elevator e => Show (Color Y' e) where
  showsPrec _ = showsColorModel

-- | `Y'` color model
instance Elevator e => ColorModel Y' e where
  type Components Y' e = e
  toComponents (Y' y) = y
  {-# INLINE toComponents #-}
  fromComponents = Y'
  {-# INLINE fromComponents #-}

-- | `Y'` color model
instance Functor (Color Y') where
  fmap f (Y' y) = Y' (f y)
  {-# INLINE fmap #-}

-- | `Y'` color model
instance Applicative (Color Y') where
  pure = Y'
  {-# INLINE pure #-}
  (Y' fy) <*> (Y' y) = Y' (fy y)
  {-# INLINE (<*>) #-}

-- | `Y'` color model
instance Foldable (Color Y') where
  foldr f !z (Y' y) = f y z
  {-# INLINE foldr #-}

-- | `Y'` color model
instance Traversable (Color Y') where
  traverse f (Y' y) = Y' <$> f y
  {-# INLINE traverse #-}



------------------
-- Luma Weights --
------------------

class Luma cs where
  {-# MINIMAL (rWeight, gWeight)|(rWeight, bWeight)|(gWeight, bWeight) #-}
  rWeight :: RealFloat e => Weight cs e
  rWeight = 1 - bWeight - gWeight
  {-# INLINE rWeight #-}

  gWeight :: RealFloat e => Weight cs e
  gWeight = 1 - rWeight - bWeight
  {-# INLINE gWeight #-}

  bWeight :: RealFloat e => Weight cs e
  bWeight = 1 - rWeight - gWeight
  {-# INLINE bWeight #-}

newtype Weight cs e = Weight
  { unWeight :: e
  } deriving (Eq, Show, Num, Fractional, Floating, Functor)


rgbLumaWeights ::
     forall cs e' e. (Luma cs, RealFloat e)
  => Color cs e'
  -> Weights e
rgbLumaWeights _ =
  Weights (V3 (coerce (rWeight :: Weight cs e) :: e)
              (coerce (gWeight :: Weight cs e) :: e)
              (coerce (bWeight :: Weight cs e) :: e))
{-# INLINE rgbLumaWeights #-}

-- | Convert a non-linear RGB pixel to a luma pixel
rgbLuma ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Color cs e'
  -> Color Y' e
rgbLuma rgb' = Y' (coerce (fmap toRealFloat rgb :: Color CM.RGB e) `dotProduct` coerce weights)
  where
    !rgb = unColorRGB rgb'
    !weights = rgbLumaWeights rgb' :: Weights e
{-# INLINE rgbLuma #-}
