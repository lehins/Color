{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
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
  , Primary(.., Primary)
  , xPrimary
  , yPrimary
  , zPrimary
  , primaryXZ
  , primaryXYZ
  , WhitePoint(.., WhitePoint)
  , Tristimulus(..)
  , normalTristimulus
  , xWhitePoint
  , yWhitePoint
  , zWhitePoint
  , whitePointXZ
  , whitePointXYZ
  , Illuminant(..)
  , CCT(..)
  , XYZ
  , pattern PixelXYZ
  , pattern PixelXYZA
  , CIExyY
  , pattern Pixelxy
  , pattern PixelxyY
  , module GHC.TypeNats
  , module Graphics.ColorModel.Internal
  ) where

import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Y
import Graphics.ColorSpace.Algebra
import Data.Typeable
import Data.Coerce
import GHC.TypeNats

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

-- | Correlated color temperature (CCT) of a white point in Kelvin
newtype CCT (i :: k) = CCT
  { unCCT :: Double
  } deriving (Eq, Show)

class (Typeable i, Typeable k, KnownNat (Temperature i)) => Illuminant (i :: k) where
  type Temperature i :: n
  whitePoint :: RealFloat e => WhitePoint i e

  colorTemperature :: CCT i
  colorTemperature = CCT (fromIntegral (natVal (Proxy :: Proxy (Temperature i))))

-- | XYZ tristimulus of the illuminant @i@, where @Y=1@
--
-- @since 0.1.0
normalTristimulus :: forall i e. (Illuminant i, Elevator e, RealFloat e) => Tristimulus i e
normalTristimulus = Tristimulus (whitePointXYZ (whitePoint :: WhitePoint i e))

newtype WhitePoint (i :: k) e = WhitePointChroma (Pixel CIExyY e)
 deriving (Eq, Show)

-- | Constructor for the most common @XYZ@ color space
pattern WhitePoint :: e -> e -> WhitePoint i e
pattern WhitePoint x y <- (coerce -> (V2 x y)) where
  WhitePoint x y = coerce (V2 x y)
{-# COMPLETE WhitePoint #-}


newtype Tristimulus i e = Tristimulus (Pixel XYZ e)
  deriving (Show, Eq, Ord, Functor, Applicative)


xWhitePoint :: WhitePoint i e -> e
xWhitePoint (coerce -> V2 x _) = x
{-# INLINE xWhitePoint #-}

yWhitePoint :: WhitePoint i e -> e
yWhitePoint (coerce -> V2 _ y) = y
{-# INLINE yWhitePoint #-}

-- | Compute @z = 1 - x - y@ of a `WhitePoint`.
zWhitePoint :: Num e => WhitePoint i e -> e
zWhitePoint wp = 1 - xWhitePoint wp - yWhitePoint wp
{-# INLINE zWhitePoint #-}

-- | Compute @XYZ@ tristimulus of a white point, where @Y = 1@
--
-- @since 0.1.0
whitePointXYZ ::
     (RealFloat e, Elevator e)
  => WhitePoint i e
     -- ^ White point that specifies @x@ and @y@
  -> Pixel XYZ e
whitePointXYZ (WhitePointChroma xyY) = toPixelXYZ xyY
{-# INLINE whitePointXYZ #-}


-- | Compute @XYZ@ tristimulus of a white point.
--
-- @since 0.1.0
whitePointXZ ::
     Fractional e
  => e
     -- ^ @Y@ value, which is usually set to @1@
  -> WhitePoint i e
     -- ^ White point that specifies @x@ and @y@
  -> Pixel XYZ e
whitePointXZ vY (coerce -> V2 x y) = PixelXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE whitePointXZ #-}

-------------
-- Primary --
-------------

newtype Primary e =
  PrimaryChroma (Pixel CIExyY e)
  deriving (Eq, Show)

-- | Constructor for the most common @XYZ@ color space
pattern Primary :: e -> e -> Primary e
pattern Primary x y <- (coerce -> V2 x y) where
  Primary x y = coerce (V2 x y)
{-# COMPLETE Primary #-}



xPrimary :: Primary e -> e
xPrimary (coerce -> V2 x _) = x
{-# INLINE xPrimary #-}

yPrimary :: Primary e -> e
yPrimary (coerce -> V2 _ y) = y
{-# INLINE yPrimary #-}

-- | Compute @z = 1 - x - y@ of a `Primary`.
zPrimary :: Num e => Primary e -> e
zPrimary p = 1 - xPrimary p - yPrimary p
{-# INLINE zPrimary #-}




-- | Compute `XYZ` tristimulus of a `Primary`, where @Y = 1@
--
-- @since 0.1.0
primaryXYZ ::
     (RealFloat e, Elevator e)
  => Primary e
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel XYZ e
primaryXYZ (PrimaryChroma xy) = toPixelXYZ xy
{-# INLINE primaryXYZ #-}

-- | Compute `XYZ` tristimulus of a `Primary`.
--
-- @since 0.1.0
primaryXZ ::
     Fractional e =>
     e
     -- ^ @Y@ value, which is usually set to @1@
  -> Primary e
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel XYZ e
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



---------------
--- CIE xyY ---
---------------

-- | The original color space CIE 1931 XYZ color space
data CIExyY

-- | CIE1931 `XYZ` color space
newtype instance Pixel CIExyY e = CIExyY (V2 e)

-- | Constructor @CIE xyY@ color space. It only requires @x@ and @y@, then @Y@ part will
-- always be equal to 1.
pattern Pixelxy :: e -> e -> Pixel CIExyY e
pattern Pixelxy x y = CIExyY (V2 x y)
{-# COMPLETE Pixelxy #-}

-- | Patttern match on the @CIE xyY@, 3rd argument @Y@ is always set to @1@
pattern PixelxyY :: Num e => e -> e -> e -> Pixel CIExyY e
pattern PixelxyY x y y' <- (addY -> V3 x y y')

addY :: Num e => Pixel CIExyY e -> V3 e
addY (CIExyY (V2 x y)) = V3 x y 1
{-# INLINE addY #-}

-- | CIE xyY color space
deriving instance Eq e => Eq (Pixel CIExyY e)

-- | CIE xyY color space
deriving instance Ord e => Ord (Pixel CIExyY e)


-- | CIE xyY color space
deriving instance Functor (Pixel CIExyY)

-- | CIE xyY color space
deriving instance Applicative (Pixel CIExyY)

-- | CIE xyY color space
deriving instance Foldable (Pixel CIExyY)

-- | CIE xyY color space
deriving instance Traversable (Pixel CIExyY)

-- | CIE xyY color space
deriving instance Storable e => Storable (Pixel CIExyY e)

-- | CIE xyY color space
instance Elevator e => Show (Pixel CIExyY e) where
  showsPrec _ = showsColorModel

-- | CIE xyY color space
instance Elevator e => ColorModel CIExyY e where
  type Components CIExyY e = (e, e)
  toComponents (CIExyY (V2 x y)) = (x, y)
  {-# INLINE toComponents #-}
  fromComponents (x, y) = CIExyY (V2 x y)
  {-# INLINE fromComponents #-}

-- | CIE xyY color space
instance Elevator e => ColorSpace CIExyY e where
  type BaseColorSpace CIExyY = CIExyY
  toBaseColorSpace = id
  fromBaseColorSpace = id
  showsColorSpaceName _ = ("CIExyY" ++)
  toPixelY _ = PixelY 1
  {-# INLINE toPixelY #-}
  toPixelXYZ xy = PixelXYZ (x / y) 1 ((1 - x - y) / y)
    where Pixelxy x y = toRealFloat <$> xy
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ xyz = fromRealFloat <$> Pixelxy (x / s) (y / s)
    where
      PixelXYZ x y z = xyz
      !s = x + y + z
  {-# INLINE fromPixelXYZ #-}
