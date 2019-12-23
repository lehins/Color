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
  , pattern ColorY'
  , Color
  , Luma(..)
  , Weight(..)
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

-- | Luma.
data Y'

-- | Constructor for Luma.
newtype instance Color Y' e = ColorY' e

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
  toComponents (ColorY' y) = y
  {-# INLINE toComponents #-}
  fromComponents = ColorY'
  {-# INLINE fromComponents #-}

-- | `Y'` color model
instance Functor (Color Y') where
  fmap f (ColorY' y) = ColorY' (f y)
  {-# INLINE fmap #-}

-- | `Y'` color model
instance Applicative (Color Y') where
  pure = ColorY'
  {-# INLINE pure #-}
  (ColorY' fy) <*> (ColorY' y) = ColorY' (fy y)
  {-# INLINE (<*>) #-}

-- | `Y'` color model
instance Foldable (Color Y') where
  foldr f !z (ColorY' y) = f y z
  {-# INLINE foldr #-}

-- | `Y'` color model
instance Traversable (Color Y') where
  traverse f (ColorY' y) = ColorY' <$> f y
  {-# INLINE traverse #-}



------------------
-- Luma Weights --
------------------

class Luma cs where
  {-# MINIMAL (rWeight, gWeight)|(rWeight, bWeight)|(gWeight, bWeight) #-}
  rWeight :: Weight cs
  rWeight = 1 - bWeight - gWeight
  {-# INLINE rWeight #-}

  gWeight :: Weight cs
  gWeight = 1 - rWeight - bWeight
  {-# INLINE gWeight #-}

  bWeight :: Weight cs
  bWeight = 1 - rWeight - gWeight
  {-# INLINE bWeight #-}

newtype Weight cs = Weight
  { unWeight :: Double
  } deriving (Eq, Show, Num, Fractional, Floating)


rgbLumaWeights ::
     forall cs e' e. (Luma cs, Elevator e, RealFloat e)
  => Color cs e'
  -> Weights e
rgbLumaWeights _ =
  Weights (V3 (toRealFloat (coerce (rWeight :: Weight cs) :: Double))
              (toRealFloat (coerce (gWeight :: Weight cs) :: Double))
              (toRealFloat (coerce (bWeight :: Weight cs) :: Double)))
{-# INLINE rgbLumaWeights #-}

-- | Convert a non-linear RGB pixel to a luma pixel
rgbLuma ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Color cs e'
  -> Color Y' e
rgbLuma rgb' = ColorY' (coerce (fmap toRealFloat rgb :: Color CM.RGB e) `dotProduct` coerce weights)
  where
    !rgb = unColorRGB rgb'
    !weights = rgbLumaWeights rgb' :: Weights e
{-# INLINE rgbLuma #-}
