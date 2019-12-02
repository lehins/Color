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
-- Module      : Graphics.ColorSpace.CIE1976.LAB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.CIE1976.LAB
  ( LAB
  -- * Constructors for an CIE L*a*b* color space.
  , pattern LAB
  , pattern PixelLAB
  , pattern PixelLABA
  , Pixel
  ) where

import Data.Proxy
import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.Internal

--------------
--- CIELAB ---
--------------

-- | CIE L*a*b* color space
data LAB (i :: k)

-- | Pixel in CIE L*a*b* color space
newtype instance Pixel (LAB i) e = LAB (V3 e)


pattern PixelLAB :: e -> e -> e -> Pixel (LAB i) e
pattern PixelLAB l' a' b' = LAB (V3 l' a' b')
{-# COMPLETE PixelLAB #-}

-- | Constructor for @LAB@ with alpha channel.
pattern PixelLABA :: e -> e -> e -> e -> Pixel (Alpha (LAB i)) e
pattern PixelLABA l' a' b' a = Alpha (LAB (V3 l' a' b')) a
{-# COMPLETE PixelLABA #-}

-- | CIE1976 `LAB` color space
deriving instance Eq e => Eq (Pixel (LAB i) e)

-- | CIE1976 `LAB` color space
deriving instance Ord e => Ord (Pixel (LAB i) e)

-- | CIE1976 `LAB` color space
deriving instance Functor (Pixel (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Applicative (Pixel (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Foldable (Pixel (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Traversable (Pixel (LAB i))

-- | CIE1976 `LAB` color space
deriving instance Storable e => Storable (Pixel (LAB i) e)

-- | CIE1976 `LAB` color space
instance (Illuminant i, Elevator e) => Show (Pixel (LAB i) e) where
  showsPrec _ = showsColorModel

-- | CIE1976 `LAB` color space
instance (Illuminant i, Elevator e) => ColorModel (LAB i) e where
  type Components (LAB i) e = (e, e, e)
  toComponents (PixelLAB l' a' b') = (l', a', b')
  {-# INLINE toComponents #-}
  fromComponents (l', a', b') = PixelLAB l' a' b'
  {-# INLINE fromComponents #-}

instance (Illuminant i, Elevator e, RealFloat e) => ColorSpace (LAB (i :: k)) i e where
  type BaseColorSpace (LAB i) = LAB i
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelY (PixelLAB l' _ _) = PixelY (ift (scaleLightness l'))
  {-# INLINE toPixelY #-}
  toPixelXYZ = lab2xyz
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = xyz2lab
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = showsType (Proxy :: Proxy (LAB i))

lab2xyz ::
     forall i a e. (Illuminant i, Elevator e, Elevator a, RealFloat a)
  => Pixel (LAB i) e
  -> Pixel (XYZ i) a
lab2xyz (PixelLAB l' a' b') = PixelXYZ x y z
  where
    !(Tristimulus (PixelXYZ wx _ wz)) = normalTristimulus :: Tristimulus i a
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
  => Pixel (XYZ i) a
  -> Pixel (LAB i) e
xyz2lab (PixelXYZ x y z) = PixelLAB l' a' b'
  where
    !(Tristimulus (PixelXYZ wx _ wz)) = normalTristimulus :: Tristimulus i e
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
