{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Internal
  ( Pixel(..)
  , ColorSpace(..)
  , Primary(..)
  , WhitePoint(..)
  , pixelWhitePoint
  , Illuminant(..)
  , Chromaticity(..)
  --, AlphaSpace(..)
  , XYZ
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Helpers


class ColorModel cs e => ColorSpace cs e where
  toPixelXYZ :: Pixel cs e -> Pixel XYZ Double
  fromPixelXYZ :: Pixel XYZ Double -> Pixel cs e

data Primary = Primary
  { xPrimary :: {-# UNPACK #-}!Double
  , yPrimary :: {-# UNPACK #-}!Double
  } deriving (Eq, Show)

data WhitePoint i = WhitePoint
  { xWhitePoint :: {-# UNPACK #-}!Double
  , yWhitePoint :: {-# UNPACK #-}!Double
  } deriving (Eq, Show)

data Chromaticity cs i where
  Chromaticity :: Illuminant i =>
    { chromaRed   :: {-# UNPACK #-}!Primary
    , chromaGreen :: {-# UNPACK #-}!Primary
    , chromaBlue  :: {-# UNPACK #-}!Primary
    } -> Chromaticity cs i
deriving instance Eq (Chromaticity cs i)
deriving instance Show (Chromaticity cs i)

class Illuminant (i :: k) where
  whitePoint :: WhitePoint i

-- | Get the white point of any pixel with color space that specifies one. itself isn't
-- actually evaluated, its type carries enough information for getting the white point.
--
-- >>> import Graphics.ColorSpace.RGB.S as SRGB
-- >>> pixelWhitePoint (PixelRGB 0.1 0.2 0.3 :: Pixel RGB Double)
-- WhitePoint {xWhitePoint = 0.3127, yWhitePoint = 0.329}
--
-- @since 0.1.0
pixelWhitePoint :: Illuminant i => Pixel (cs i) e -> WhitePoint i
pixelWhitePoint _ = whitePoint
{-# INLINE pixelWhitePoint #-}

-----------
--- XYZ ---
-----------

-- | The original color space @CIE 1931 XYZ@ color space
data XYZ

data instance Pixel XYZ e = PixelXYZ !e !e !e deriving (Eq, Ord, Bounded)

instance Show e => Show (Pixel XYZ e) where
  showsPrec _ (PixelXYZ x y z) = showsP "XYZ" (shows3 x y z)

instance Elevator e => ColorModel XYZ e where
  type Components XYZ e = (e, e, e)
  toComponents (PixelXYZ x y z) = (x, y, z)
  {-# INLINE toComponents #-}
  fromComponents (x, y, z) = PixelXYZ x y z
  {-# INLINE fromComponents #-}


instance Elevator e => ColorSpace XYZ e where
  toPixelXYZ (PixelXYZ x y z) = PixelXYZ (toDouble x) (toDouble y) (toDouble z)
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ (PixelXYZ x y z) = PixelXYZ (fromDouble x) (fromDouble y) (fromDouble z)
  {-# INLINE fromPixelXYZ #-}

{-# RULES
"toPixelXYZ :: Pixel XYZ Double -> Pixel XYZ Double"   toPixelXYZ = id
"fromPixelXYZ :: Pixel XYZ Double -> Pixel XYZ Double" fromPixelXYZ = id
 #-}

instance Functor (Pixel XYZ) where
  fmap f (PixelXYZ x y z) = PixelXYZ (f x) (f y) (f z)
  {-# INLINE fmap #-}

instance Applicative (Pixel XYZ) where
  pure e = PixelXYZ e e e
  {-# INLINE pure #-}
  (PixelXYZ fx fy fz) <*> (PixelXYZ x y z) = PixelXYZ (fx x) (fy y) (fz z)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel XYZ) where
  foldr f acc (PixelXYZ x y z) = foldr3 f acc x y z
  {-# INLINE foldr #-}

instance Traversable (Pixel XYZ) where
  traverse f (PixelXYZ x y z) = traverse3 PixelXYZ f x y z
  {-# INLINE traverse #-}

instance Storable e => Storable (Pixel XYZ e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelXYZ
  {-# INLINE peek #-}
  poke p (PixelXYZ x y z) = poke3 p x y z
  {-# INLINE poke #-}
