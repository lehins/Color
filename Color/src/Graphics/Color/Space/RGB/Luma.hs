{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Luma
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Luma
  ( -- * Luma
    pattern Y'
  , pattern Y'A
  , pattern Luma
  , Y'
  , Luma(..)
  , Weight(..)
  , Weights(..)
  , rgbLuma
  , rgbLumaWeights
  , toBaseLinearSpace
  ) where

import Data.Coerce
import Data.Kind
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB as CM
import Graphics.Color.Model.Y as CM
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal

-------------
--- Luma ----
-------------

-- | [Luma](https://en.wikipedia.org/wiki/Luma_(video\)) of a non-linear gamma corrected
-- RGB color space. (Not to be confused with luminance `Y`)
data Y' (cs :: Linearity -> Type)

-- | Constructor for Luma.
newtype instance Color (Y' cs) e = Luma (CM.Color CM.Y e)

-- | Constructor for Luma `Y'`. (Not to be confused with luminance `Y`)
--
-- @since 0.1.0
pattern Y' :: e -> Color (Y' cs) e
pattern Y' y = Luma (CM.Y y)
{-# COMPLETE Y' #-}

-- | Constructor for `Y'` with alpha channel. (Not to be confused with luminance `Y`)
--
-- @since 0.1.4
pattern Y'A :: e -> e -> Color (Alpha (Y' cs)) e
pattern Y'A y a = Alpha (Luma (CM.Y y)) a
{-# COMPLETE Y'A #-}

-- | `Y'` - luma of a color space
deriving instance Eq e => Eq (Color (Y' cs) e)
-- | `Y'` - luma of a color space
deriving instance Ord e => Ord (Color (Y' cs) e)
-- | `Y'` - luma of a color space
deriving instance Functor (Color (Y' cs))
-- | `Y'` - luma of a color space
deriving instance Applicative (Color (Y' cs))
-- | `Y'` - luma of a color space
deriving instance Foldable (Color (Y' cs))
-- | `Y'` - luma of a color space
deriving instance Traversable (Color (Y' cs))
-- | `Y'` - luma of a color space
deriving instance Storable e => Storable (Color (Y' cs) e)


-- | `Y'` - as a color model
instance (Typeable cs, Elevator e) => Show (Color (Y' cs) e) where
  showsPrec _ = showsColorModel

-- | `Y'` - as a color model
instance (Typeable cs, Elevator e) => ColorModel (Y' cs) e where
  type Components (Y' cs) e = e
  toComponents (Y' y) = y
  {-# INLINE toComponents #-}
  fromComponents = Y'
  {-# INLINE fromComponents #-}


instance ( Typeable cs
         , Illuminant i
         , ColorSpace (cs 'Linear) i e
         , ColorSpace (cs 'NonLinear) i e
         , Luma cs
         , RedGreenBlue cs i
         , RealFloat e
         ) =>
         ColorSpace (Y' cs) i e where
  type BaseModel (Y' cs) = CM.Y
  type BaseSpace (Y' cs) = cs 'NonLinear
  toBaseSpace y = pure (coerce y :: e)
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = rgbLuma
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseLinearSpace
  {-# INLINE luminance #-}
  toColorXYZ = toColorXYZ . toBaseLinearSpace
  {-# INLINE toColorXYZ #-}

-- | Convert Luma directly into the linear version of base space. This is equivalent to
-- `dcctf . toBaseSpace`, but is a bit faster, since inverse transfer function is applied
-- only once
--
-- @since 0.3.0
toBaseLinearSpace ::
     forall cs e i. (RedGreenBlue cs i, Applicative (Color (cs 'Linear)), RealFloat e)
  => Color (Y' cs) e
  -> Color (cs 'Linear) e
toBaseLinearSpace y = pure (itransfer @_ @cs (coerce y :: e))
{-# INLINE toBaseLinearSpace #-}

------------------
-- Luma Weights --
------------------

class Luma (cs :: Linearity -> Type) where
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

-- | Get the weights of a non-linear RGB color space that can be used for converting to `Luma`
--
-- @since 0.1.4
rgbLumaWeights ::
     forall cs e' e. (Luma cs, RealFloat e)
  => Color (cs 'NonLinear) e'
  -> Weights e
rgbLumaWeights _ =
  Weights (V3 (coerce (rWeight :: Weight cs e) :: e)
              (coerce (gWeight :: Weight cs e) :: e)
              (coerce (bWeight :: Weight cs e) :: e))
{-# INLINE rgbLumaWeights #-}

-- | Convert a non-linear RGB pixel to a luma pixel
--
-- @since 0.1.0
rgbLuma ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Color (cs 'NonLinear) e'
  -> Color (Y' cs) e
rgbLuma rgb' = Y' (coerce (fmap toRealFloat rgb :: Color CM.RGB e) `dotProduct` coerce weights)
  where
    !rgb = unColorRGB rgb'
    !weights = rgbLumaWeights rgb' :: Weights e
{-# INLINE rgbLuma #-}
