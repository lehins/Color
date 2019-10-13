{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , zPrimary
  , primaryXZ
  , primaryXYZ
  , WhitePoint(..)
  , zWhitePoint
  , whitePointXZ
  , whitePointXYZ
  , pixelWhitePoint
  , Illuminant(..)
  , XYZ
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Algebra
import Data.Typeable

class ColorModel cs e => ColorSpace cs e where
  type BaseColorSpace cs :: *

  toBaseColorSpace :: Pixel cs e -> Pixel (BaseColorSpace cs) e
  fromBaseColorSpace :: Pixel (BaseColorSpace cs) e -> Pixel cs e

  -- | Display the official color space name with any extra parameters. Pixel itself will
  -- not be evaluated.
  --
  -- @since 0.1.0
  showsColorSpaceName :: Pixel cs e -> ShowS

  toPixelXYZ :: Pixel cs e -> Pixel XYZ Double
  default toPixelXYZ :: ColorSpace (BaseColorSpace cs) e => Pixel cs e -> Pixel XYZ Double
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}

  fromPixelXYZ :: Pixel XYZ Double -> Pixel cs e
  default fromPixelXYZ :: ColorSpace (BaseColorSpace cs) e => Pixel XYZ Double -> Pixel cs e
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}


----------------
-- WhitePoint --
----------------

class (Typeable i, Typeable k) => Illuminant (i :: k) where
  whitePoint :: WhitePoint i

data WhitePoint (i :: k) = WhitePoint
  { xWhitePoint :: {-# UNPACK #-}!Double
  , yWhitePoint :: {-# UNPACK #-}!Double
  } deriving (Eq, Show)

zWhitePoint :: WhitePoint i -> Double
zWhitePoint wp = 1 - xWhitePoint wp - yWhitePoint wp
{-# INLINE zWhitePoint #-}

-- | Compute @XYZ@ tristimulus of a white point, where @Y = 1@
--
-- @since 0.1.0
whitePointXYZ ::
     WhitePoint i
     -- ^ White point that specifies @x@ and @y@
  -> V3
whitePointXYZ = whitePointXZ 1
{-# INLINE whitePointXYZ #-}


-- | Compute @XYZ@ tristimulus of a white point.
--
-- @since 0.1.0
whitePointXZ :: Double
              -- ^ @Y@ value, which is usually set to @1@
              -> WhitePoint i
              -- ^ White point that specifies @x@ and @y@
              -> V3
whitePointXZ vY (WhitePoint x y) = V3 (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE whitePointXZ #-}

-------------
-- Primary --
-------------

data Primary = Primary
  { xPrimary :: {-# UNPACK #-}!Double
  , yPrimary :: {-# UNPACK #-}!Double
  } deriving (Eq, Show)


-- | Compute @z = 1 - x - y@ of a `Primary`.
zPrimary :: Primary -> Double
zPrimary p = 1 - xPrimary p - yPrimary p
{-# INLINE zPrimary #-}




-- | Compute @XYZ@ tristimulus of a Primary, where @Y = 1@
--
-- @since 0.1.0
primaryXYZ ::
     Primary
     -- ^ Primary that specifies @x@ and @y@
  -> V3
primaryXYZ = primaryXZ 1
{-# INLINE primaryXYZ #-}

-- | Compute @XYZ@ tristimulus of a Primary.
--
-- @since 0.1.0
primaryXZ ::
     Double
     -- ^ @Y@ value, which is usually set to @1@
  -> Primary
     -- ^ Primary that specifies @x@ and @y@
  -> V3
primaryXZ vY (Primary x y) = V3 (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE primaryXZ #-}


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

-- | The original color space CIE 1931 XYZ color space
data XYZ

-- | CIE1931 `XYZ` color space
data instance Pixel XYZ e = PixelXYZ !e !e !e

-- | CIE1931 `XYZ` color space
deriving instance Eq e => Eq (Pixel XYZ e)

-- | CIE1931 `XYZ` color space
deriving instance Ord e => Ord (Pixel XYZ e)

-- | CIE1931 `XYZ` color space
instance Elevator e => Show (Pixel XYZ e) where
  showsPrec _ = showsColorModel

-- | CIE1931 `XYZ` color space
instance Elevator e => ColorModel XYZ e where
  type Components XYZ e = (e, e, e)
  toComponents (PixelXYZ x y z) = (x, y, z)
  {-# INLINE toComponents #-}
  fromComponents (x, y, z) = PixelXYZ x y z
  {-# INLINE fromComponents #-}

-- | CIE1931 `XYZ` color space
instance Elevator e => ColorSpace XYZ e where
  type BaseColorSpace XYZ = XYZ
  toBaseColorSpace = id
  fromBaseColorSpace = id
  showsColorSpaceName _ = ("CIE1931 XYZ" ++)
  toPixelXYZ (PixelXYZ x y z) = PixelXYZ (toDouble x) (toDouble y) (toDouble z)
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ (PixelXYZ x y z) = PixelXYZ (fromDouble x) (fromDouble y) (fromDouble z)
  {-# INLINE fromPixelXYZ #-}

{-# RULES
"toPixelXYZ   :: Pixel XYZ Double -> Pixel XYZ Double"   toPixelXYZ = id
"fromPixelXYZ :: Pixel XYZ Double -> Pixel XYZ Double" fromPixelXYZ = id
 #-}

-- | CIE1931 `XYZ` color space
instance Functor (Pixel XYZ) where
  fmap f (PixelXYZ x y z) = PixelXYZ (f x) (f y) (f z)
  {-# INLINE fmap #-}

-- | CIE1931 `XYZ` color space
instance Applicative (Pixel XYZ) where
  pure e = PixelXYZ e e e
  {-# INLINE pure #-}
  (PixelXYZ fx fy fz) <*> (PixelXYZ x y z) = PixelXYZ (fx x) (fy y) (fz z)
  {-# INLINE (<*>) #-}

-- | CIE1931 `XYZ` color space
instance Foldable (Pixel XYZ) where
  foldr f acc (PixelXYZ x y z) = foldr3 f acc x y z
  {-# INLINE foldr #-}

-- | CIE1931 `XYZ` color space
instance Traversable (Pixel XYZ) where
  traverse f (PixelXYZ x y z) = traverse3 PixelXYZ f x y z
  {-# INLINE traverse #-}

-- | CIE1931 `XYZ` color space
instance Storable e => Storable (Pixel XYZ e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelXYZ
  {-# INLINE peek #-}
  poke p (PixelXYZ x y z) = poke3 p x y z
  {-# INLINE poke #-}
