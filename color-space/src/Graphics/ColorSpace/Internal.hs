{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
  , Tristimulus(..)
  , tristimulus
  , zWhitePoint
  , whitePointXZ
  , whitePointXYZ
  , Illuminant(..)
  , XYZ
  , pattern PixelXYZ
  , pattern PixelXYZA
  ) where

import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Y
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

  -- | Get pixel luminocity
  --
  -- @since 0.1.0
  toPixelY :: (Elevator a, RealFloat a) => Pixel cs e -> Pixel Y a
  default toPixelY ::
    (ColorSpace (BaseColorSpace cs) e, Elevator a, RealFloat a) => Pixel cs e -> Pixel Y a
  toPixelY = toPixelY . toBaseColorSpace
  {-# INLINE toPixelY #-}

  toPixelXYZ :: (Elevator a, RealFloat a) => Pixel cs e -> Pixel XYZ a
  default toPixelXYZ ::
    (ColorSpace (BaseColorSpace cs) e, Elevator a, RealFloat a) => Pixel cs e -> Pixel XYZ a
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}

  fromPixelXYZ :: (Elevator a, RealFloat a) => Pixel XYZ a -> Pixel cs e
  default fromPixelXYZ ::
    (ColorSpace (BaseColorSpace cs) e, Elevator a, RealFloat a) => Pixel XYZ a -> Pixel cs e
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}

----------------
-- WhitePoint --
----------------

class (Typeable i, Typeable k) => Illuminant (i :: k)
  where
  whitePoint :: WhitePoint i

tristimulus :: forall i e. (Illuminant i, Elevator e, RealFloat e) => Tristimulus i e
tristimulus = Tristimulus (toRealFloat <$> PixelXYZ (wx / wy) 1 ((1 - wx - wy) / wy))
  where
    WhitePoint wx wy = whitePoint :: WhitePoint i

data WhitePoint (i :: k) = WhitePoint
  { xWhitePoint :: {-# UNPACK #-}!Double
  , yWhitePoint :: {-# UNPACK #-}!Double
  } deriving (Eq, Show)

newtype Tristimulus i e = Tristimulus (Pixel XYZ e)
  deriving (Show, Eq, Ord, Functor, Applicative)


zWhitePoint :: WhitePoint i -> Double
zWhitePoint wp = 1 - xWhitePoint wp - yWhitePoint wp
{-# INLINE zWhitePoint #-}

-- | Compute @XYZ@ tristimulus of a white point, where @Y = 1@
--
-- @since 0.1.0
whitePointXYZ ::
     WhitePoint i
     -- ^ White point that specifies @x@ and @y@
  -> Pixel XYZ Double
whitePointXYZ = whitePointXZ 1
{-# INLINE whitePointXYZ #-}


-- | Compute @XYZ@ tristimulus of a white point.
--
-- @since 0.1.0
whitePointXZ :: Double
              -- ^ @Y@ value, which is usually set to @1@
              -> WhitePoint i
              -- ^ White point that specifies @x@ and @y@
              -> Pixel XYZ Double
whitePointXZ vY (WhitePoint x y) = PixelXYZ (vYy * x) vY (vYy * (1 - x - y))
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




-- | Compute `XYZ` tristimulus of a `Primary`, where @Y = 1@
--
-- @since 0.1.0
primaryXYZ ::
     Primary
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel XYZ Double
primaryXYZ = primaryXZ 1
{-# INLINE primaryXYZ #-}

-- | Compute `XYZ` tristimulus of a `Primary`.
--
-- @since 0.1.0
primaryXZ ::
     Double
     -- ^ @Y@ value, which is usually set to @1@
  -> Primary
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel XYZ Double
primaryXZ vY (Primary x y) = PixelXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE primaryXZ #-}


-----------
--- XYZ ---
-----------

-- | The original color space CIE 1931 XYZ color space
data XYZ

-- | CIE1931 `XYZ` color space
newtype instance Pixel XYZ e = XYZ (V3 e)

-- | Constructor for the most common @XYZ@ color space
pattern PixelXYZ :: e -> e -> e -> Pixel XYZ e
pattern PixelXYZ x y z = XYZ (V3 x y z)
{-# COMPLETE PixelXYZ #-}

-- | Constructor for @XYZ@ with alpha channel.
pattern PixelXYZA :: e -> e -> e -> e -> Pixel (Alpha XYZ) e
pattern PixelXYZA x y z a = Alpha (XYZ (V3 x y z)) a
{-# COMPLETE PixelXYZA #-}


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
  toPixelY (PixelXYZ _ y _) = PixelY (toRealFloat y)
  {-# INLINE toPixelY #-}
  toPixelXYZ (PixelXYZ x y z) = PixelXYZ (toRealFloat x) (toRealFloat y) (toRealFloat z)
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ (PixelXYZ x y z) = PixelXYZ (fromRealFloat x) (fromRealFloat y) (fromRealFloat z)
  {-# INLINE fromPixelXYZ #-}

{-# RULES
"toPixelXYZ   :: RealFloat a => Pixel XYZ a -> Pixel XYZ a"   toPixelXYZ = id
"fromPixelXYZ :: RealFloat a => Pixel XYZ a -> Pixel XYZ a" fromPixelXYZ = id
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
