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
-- Module      : Graphics.ColorSpace.RGB.Luma
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Luma
  ( Y'
  -- * Constructors for Luma.
  , pattern PixelY'
  , Pixel
  , Luma(..)
  , Weight(..)
  , rgbLuma
  , rgbLumaWeights
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Y
import Graphics.ColorSpace.RGB.Internal
import Graphics.ColorSpace.Algebra

-------------
--- Luma ----
-------------

-- | Luma.
data Y'

-- | Constructor for Luma.
newtype instance Pixel Y' e = PixelY' e

-- | `Y'` color model
deriving instance Eq e => Eq (Pixel Y' e)
-- | `Y'` color model
deriving instance Ord e => Ord (Pixel Y' e)
-- | `Y'` color model
deriving instance Storable e => Storable (Pixel Y' e)


-- | `Y'` color model
instance Elevator e => Show (Pixel Y' e) where
  showsPrec _ = showsColorModel

-- | `Y'` color model
instance Elevator e => ColorModel Y' e where
  type Components Y' e = e
  toComponents (PixelY' y) = y
  {-# INLINE toComponents #-}
  fromComponents = PixelY'
  {-# INLINE fromComponents #-}

-- | `Y'` color model
instance Functor (Pixel Y') where
  fmap f (PixelY' y) = PixelY' (f y)
  {-# INLINE fmap #-}

-- | `Y'` color model
instance Applicative (Pixel Y') where
  pure = PixelY'
  {-# INLINE pure #-}
  (PixelY' fy) <*> (PixelY' y) = PixelY' (fy y)
  {-# INLINE (<*>) #-}

-- | `Y'` color model
instance Foldable (Pixel Y') where
  foldr f !z (PixelY' y) = f y z
  {-# INLINE foldr #-}

-- | `Y'` color model
instance Traversable (Pixel Y') where
  traverse f (PixelY' y) = PixelY' <$> f y
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
  => Pixel cs e'
  -> Weights e
rgbLumaWeights _ =
  Weights (V3 (toRealFloat (coerce (rWeight :: Weight cs) :: Double))
              (toRealFloat (coerce (gWeight :: Weight cs) :: Double))
              (toRealFloat (coerce (bWeight :: Weight cs) :: Double)))
{-# INLINE rgbLumaWeights #-}

-- | Convert a non-linear RGB pixel to a luma pixel
rgbLuma ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Pixel cs e'
  -> Pixel Y' e
rgbLuma rgb' = PixelY' (coerce (fmap toRealFloat rgb :: Pixel CM.RGB e) `dotProduct` coerce weights)
  where
    !rgb = unPixelRGB rgb'
    !weights = rgbLumaWeights rgb' :: Weights e
{-# INLINE rgbLuma #-}
