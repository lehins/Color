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
-- Module      : Graphics.Color.Space.CIE1976.LAB
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.CIE1976.LAB
  ( -- * Constructors for an CIE L*a*b* color space.
    pattern LAB
  , pattern ColorLAB
  , pattern ColorLABA
  , LAB
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

--------------
--- CIELAB ---
--------------

-- | [CIE L*a*b*](https://en.wikipedia.org/wiki/CIELAB_color_space) color space
data LAB (i :: k)

-- | Color in CIE L*a*b* color space
newtype instance Color (LAB i) e = LAB (V3 e)


pattern ColorLAB :: e -> e -> e -> Color (LAB i) e
pattern ColorLAB l' a' b' = LAB (V3 l' a' b')
{-# COMPLETE ColorLAB #-}

-- | Constructor for @LAB@ with alpha channel.
pattern ColorLABA :: e -> e -> e -> e -> Color (Alpha (LAB i)) e
pattern ColorLABA l' a' b' a = Alpha (LAB (V3 l' a' b')) a
{-# COMPLETE ColorLABA #-}

-- | CIE1976 `LAB` color space
deriving instance Eq e => Eq (Color (LAB i) e)

-- | CIE1976 `LAB` color space
deriving instance Ord e => Ord (Color (LAB i) e)

-- | CIE1976 `LAB` color space
deriving instance Functor (Color (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Applicative (Color (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Foldable (Color (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Traversable (Color (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Storable e => Storable (Color (LAB i) e)

-- | CIE1976 `LAB` color space
instance (Illuminant i, Elevator e) => Show (Color (LAB i) e) where
  showsPrec _ = showsColorModel

-- | CIE1976 `LAB` color space
instance (Illuminant i, Elevator e) => ColorModel (LAB i) e where
  type Components (LAB i) e = (e, e, e)
  toComponents (ColorLAB l' a' b') = (l', a', b')
  {-# INLINE toComponents #-}
  fromComponents (l', a', b') = ColorLAB l' a' b'
  {-# INLINE fromComponents #-}

instance (Illuminant i, Elevator e, RealFloat e) => ColorSpace (LAB (i :: k)) i e where
  type BaseModel (LAB i) = LAB i
  type BaseSpace (LAB i) = LAB i
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance (ColorLAB l' _ _) = Y (ift (scaleLightness l'))
  {-# INLINE luminance #-}
  toColorXYZ = lab2xyz
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = xyz2lab
  {-# INLINE fromColorXYZ #-}

lab2xyz ::
     forall i a e. (Illuminant i, Elevator e, Elevator a, RealFloat a)
  => Color (LAB i) e
  -> Color (XYZ i) a
lab2xyz (ColorLAB l' a' b') = ColorXYZ x y z
  where
    !(ColorXYZ wx _ wz) = whitePointTristimulus :: Color (XYZ i) a
    !l = scaleLightness l'
    !x = wx * ift (l + toRealFloat a' / 500)
    !y = ift l
    !z = wz * ift (l - toRealFloat b' / 200)
{-# INLINE lab2xyz #-}

scaleLightness :: (Elevator e, Elevator a, RealFloat a) => e -> a
scaleLightness l' = (toRealFloat l' + 16) / 116
{-# INLINE scaleLightness #-}

ift :: (Fractional a, Ord a) => a -> a
ift t
  | t > 6 / 29 = t ^ (3 :: Int)
  | otherwise = (108 / 841) * (t - 4 / 29)



xyz2lab ::
     forall i a e. (Illuminant i, Elevator a, Elevator e, RealFloat e)
  => Color (XYZ i) a
  -> Color (LAB i) e
xyz2lab (ColorXYZ x y z) = ColorLAB l' a' b'
  where
    !(ColorXYZ wx _ wz) = whitePointTristimulus :: Color (XYZ i) e
    !fx = ft (toRealFloat x / wx)
    !fy = ft (toRealFloat y)
    !fz = ft (toRealFloat z / wz)
    !l' = 116 * fy - 16
    !a' = 500 * (fx - fy)
    !b' = 200 * (fy - fz)
{-# INLINE xyz2lab #-}

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
