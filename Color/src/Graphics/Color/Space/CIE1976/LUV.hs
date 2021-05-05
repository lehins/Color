{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.CIE1976.LUV
--
module Graphics.Color.Space.CIE1976.LUV
  ( -- * Constructors for an CIE L*u*v* color space.
    pattern LUV
  , pattern ColorLUV
  , pattern ColorLUVA
  , LUV
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

--------------
--- CIELUV ---
--------------

-- | [CIE L*u*v*](https://en.wikipedia.org/wiki/CIELUV_color_space) color space
data LUV (i :: k)

-- | Color in CIE L*u*v* color space
newtype instance Color (LUV i) e = LUV (V3 e)


pattern ColorLUV :: e -> e -> e -> Color (LUV i) e
pattern ColorLUV l' u' v' = LUV (V3 l' u' v')
{-# COMPLETE ColorLUV #-}

-- | Constructor for @LUV@ with alpha channel.
pattern ColorLUVA :: e -> e -> e -> e -> Color (Alpha (LUV i)) e
pattern ColorLUVA l' u' v' a = Alpha (LUV (V3 l' u' v')) a
{-# COMPLETE ColorLUVA #-}

-- | CIE1976 `LUV` color space
deriving instance Eq e => Eq (Color (LUV i) e)

-- | CIE1976 `LUV` color space
deriving instance Ord e => Ord (Color (LUV i) e)

-- | CIE1976 `LUV` color space
deriving instance Functor (Color (LUV i))

-- | CIE1976 `LUV` color space
deriving instance Applicative (Color (LUV i))

-- | CIE1976 `LUV` color space
deriving instance Foldable (Color (LUV i))

-- | CIE1976 `LUV` color space
deriving instance Traversable (Color (LUV i))

-- | CIE1976 `LUV` color space
deriving instance Storable e => Storable (Color (LUV i) e)

-- | CIE1976 `LUV` color space
instance (Illuminant i, Elevator e) => Show (Color (LUV i) e) where
  showsPrec _ = showsColorModel

-- | CIE1976 `LUV` color space
instance (Illuminant i, Elevator e) => ColorModel (LUV i) e where
  type Components (LUV i) e = (e, e, e)
  toComponents (ColorLUV l' u' v') = (l', u', v')
  {-# INLINE toComponents #-}
  fromComponents (l', u', v') = ColorLUV l' u' v'
  {-# INLINE fromComponents #-}

instance (Illuminant i, Elevator e, RealFloat e) => ColorSpace (LUV (i :: k)) i e where
  type BaseModel (LUV i) = LUV i
  type BaseSpace (LUV i) = LUV i
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance (ColorLUV l' _ _) = Y (ift (scaleLightness l'))
  {-# INLINE luminance #-}
  toColorXYZ = luv2xyz
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = xyz2luv
  {-# INLINE fromColorXYZ #-}

luv2xyz ::
     forall i a e. (Illuminant i, Elevator e, Elevator a, RealFloat a)
  => Color (LUV i) e
  -> Color (XYZ i) a
luv2xyz (ColorLUV l' u' v') = ColorXYZ x y z
  where
    !(ColorXYZ wx _ wz) = whitePointTristimulus :: Color (XYZ i) a
    !y = ift . scaleLightness $ l'
    !wxyz = wx + 15 + 3 * wz
    !l1 = 13 * toRealFloat l'
    !a = (1/3) * ((4 * l1 / (toRealFloat u' + l1 * 4 * (wx / wxyz))) - 1) :: a
    !b = -5 * y
    !c = -1 / 3
    !d = y * (3 * l1 / (toRealFloat v' + l1 * 9 * (1 / wxyz)) - 5) :: a
    !x = (d - b) / (a - c)
    !z = x * a + b
{-# INLINE luv2xyz #-}

scaleLightness :: (Elevator e, Elevator a, RealFloat a) => e -> a
scaleLightness l' = (toRealFloat l' + 16) / 116
{-# INLINE scaleLightness #-}

ift :: (Fractional a, Ord a) => a -> a
ift t
  | t > 6 / 29 = t ^ (3 :: Int)
  | otherwise = (108 / 841) * (t - 4 / 29)


xyz2luv ::
     forall i a e. (Illuminant i, Elevator a, Elevator e, RealFloat e)
  => Color (XYZ i) a
  -> Color (LUV i) e
xyz2luv (ColorXYZ x y z) = ColorLUV l' u' v'
  where
    !l' = 116 * ft (toRealFloat y) - 16
    !(ColorXYZ wx _ wz) = whitePointTristimulus :: Color (XYZ i) e
    !xyz = toRealFloat $ x + 15 * y + 3 * z
    !wxyz = wx + 15 + 3 * wz
    !u' = 13 * l' * 4 * (toRealFloat x / xyz - wx / wxyz)
    !v' = 13 * l' * 9 * (toRealFloat y / xyz - 1 / wxyz)
{-# INLINE xyz2luv #-}

ft :: RealFloat a => a -> a
ft t
  | t > t0 = t ** (1 / 3)
  | otherwise = t * m + 4 / 29
{-# INLINE ft #-}

m :: RealFloat a => a
m = 841 / 108

t0 :: RealFloat a => a
t0 = 216 / 24389
  -- where
  --   m = 1/3 * δ^-2 = 841/108 =~ 7.787[037]
  --   t0 = δ^3 =~ 0.008856
  --   δ = 6/29 =~ 0.2069
