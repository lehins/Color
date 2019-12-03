{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Kind

class (Illuminant i, ColorModel cs e) => ColorSpace cs (i :: k) e | cs -> i where

  type BaseColorSpace cs :: Type
  type BaseColorSpace cs = cs

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
    (ColorSpace (BaseColorSpace cs) i e, Elevator a, RealFloat a) => Pixel cs e -> Pixel Y a
  toPixelY = toPixelY . toBaseColorSpace
  {-# INLINE toPixelY #-}

  toPixelXYZ :: (Elevator a, RealFloat a) => Pixel cs e -> Pixel (XYZ i) a
  default toPixelXYZ ::
    (ColorSpace (BaseColorSpace cs) i e, Elevator a, RealFloat a) => Pixel cs e -> Pixel (XYZ i) a
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}

  fromPixelXYZ :: (Elevator a, RealFloat a) => Pixel (XYZ i) a -> Pixel cs e
  default fromPixelXYZ ::
    (ColorSpace (BaseColorSpace cs) i e, Elevator a, RealFloat a) => Pixel (XYZ i) a -> Pixel cs e
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

newtype WhitePoint (i :: k) e = WhitePointChroma (Pixel (CIExyY i) e)
 deriving (Eq, Show)

-- | Constructor for the most common @XYZ@ color space
pattern WhitePoint :: e -> e -> WhitePoint i e
pattern WhitePoint x y <- (coerce -> (V2 x y)) where
  WhitePoint x y = coerce (V2 x y)
{-# COMPLETE WhitePoint #-}


newtype Tristimulus i e = Tristimulus (Pixel (XYZ i) e)
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
     (Illuminant i, RealFloat e, Elevator e)
  => WhitePoint i e
     -- ^ White point that specifies @x@ and @y@
  -> Pixel (XYZ i) e
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
  -> Pixel (XYZ i) e
whitePointXZ vY (coerce -> V2 x y) = PixelXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE whitePointXZ #-}

-------------
-- Primary --
-------------

newtype Primary i e =
  PrimaryChroma (Pixel (CIExyY i) e)
  deriving (Eq, Show)

-- | Constructor for the most common @XYZ@ color space
pattern Primary :: e -> e -> Primary i e
pattern Primary x y <- (coerce -> V2 x y) where
  Primary x y = coerce (V2 x y)
{-# COMPLETE Primary #-}



xPrimary :: Primary i e -> e
xPrimary (coerce -> V2 x _) = x
{-# INLINE xPrimary #-}

yPrimary :: Primary i e -> e
yPrimary (coerce -> V2 _ y) = y
{-# INLINE yPrimary #-}

-- | Compute @z = 1 - x - y@ of a `Primary`.
zPrimary :: Num e => Primary i e -> e
zPrimary p = 1 - xPrimary p - yPrimary p
{-# INLINE zPrimary #-}




-- | Compute `XYZ` tristimulus of a `Primary`, where @Y = 1@
--
-- @since 0.1.0
primaryXYZ ::
     (Illuminant i, RealFloat e, Elevator e)
  => Primary i e
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel (XYZ i) e
primaryXYZ (PrimaryChroma xy) = toPixelXYZ xy
{-# INLINE primaryXYZ #-}

-- | Compute `XYZ` tristimulus of a `Primary`.
--
-- @since 0.1.0
primaryXZ ::
     Fractional e =>
     e
     -- ^ @Y@ value, which is usually set to @1@
  -> Primary i e
     -- ^ Primary that specifies @x@ and @y@
  -> Pixel (XYZ i) e
primaryXZ vY (Primary x y) = PixelXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE primaryXZ #-}


-----------
--- XYZ ---
-----------

-- | The original color space CIE 1931 XYZ color space
data XYZ i

-- | CIE1931 `XYZ` color space
newtype instance Pixel (XYZ i) e = XYZ (V3 e)

-- | Constructor for the most common @XYZ@ color space
pattern PixelXYZ :: e -> e -> e -> Pixel (XYZ i) e
pattern PixelXYZ x y z = XYZ (V3 x y z)
{-# COMPLETE PixelXYZ #-}

-- | Constructor for @XYZ@ with alpha channel.
pattern PixelXYZA :: e -> e -> e -> e -> Pixel (Alpha (XYZ i)) e
pattern PixelXYZA x y z a = Alpha (XYZ (V3 x y z)) a
{-# COMPLETE PixelXYZA #-}


-- | CIE1931 `XYZ` color space
deriving instance Eq e => Eq (Pixel (XYZ i) e)

-- | CIE1931 `XYZ` color space
deriving instance Ord e => Ord (Pixel (XYZ i) e)

-- | CIE1931 `XYZ` color space
instance (Typeable i, Typeable k, Elevator e) => Show (Pixel (XYZ (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | CIE1931 `XYZ` color space
instance (Typeable i, Typeable k, Elevator e) => ColorModel (XYZ (i :: k)) e where
  type Components (XYZ i) e = (e, e, e)
  toComponents (PixelXYZ x y z) = (x, y, z)
  {-# INLINE toComponents #-}
  fromComponents (x, y, z) = PixelXYZ x y z
  {-# INLINE fromComponents #-}

-- | CIE1931 `XYZ` color space
instance (Illuminant i, Elevator e) => ColorSpace (XYZ i) i e where
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
instance Functor (Pixel (XYZ i)) where
  fmap f (PixelXYZ x y z) = PixelXYZ (f x) (f y) (f z)
  {-# INLINE fmap #-}

-- | CIE1931 `XYZ` color space
instance Applicative (Pixel (XYZ i)) where
  pure e = PixelXYZ e e e
  {-# INLINE pure #-}
  (PixelXYZ fx fy fz) <*> (PixelXYZ x y z) = PixelXYZ (fx x) (fy y) (fz z)
  {-# INLINE (<*>) #-}

-- | CIE1931 `XYZ` color space
instance Foldable (Pixel (XYZ i)) where
  foldr f acc (PixelXYZ x y z) = foldr3 f acc x y z
  {-# INLINE foldr #-}

-- | CIE1931 `XYZ` color space
instance Traversable (Pixel (XYZ i)) where
  traverse f (PixelXYZ x y z) = traverse3 PixelXYZ f x y z
  {-# INLINE traverse #-}

-- | CIE1931 `XYZ` color space
instance Storable e => Storable (Pixel (XYZ i) e) where
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

-- | Alternative representation of the CIE 1931 XYZ color space
data CIExyY i

-- | CIE1931 `CIExyY` color space
newtype instance Pixel (CIExyY i) e = CIExyY (V2 e)

-- | Constructor @CIE xyY@ color space. It only requires @x@ and @y@, then @Y@ part will
-- always be equal to 1.
pattern Pixelxy :: e -> e -> Pixel (CIExyY i) e
pattern Pixelxy x y = CIExyY (V2 x y)
{-# COMPLETE Pixelxy #-}

-- | Patttern match on the @CIE xyY@, 3rd argument @Y@ is always set to @1@
pattern PixelxyY :: Num e => e -> e -> e -> Pixel (CIExyY i) e
pattern PixelxyY x y y' <- (addY -> V3 x y y')

addY :: Num e => Pixel (CIExyY i) e -> V3 e
addY (CIExyY (V2 x y)) = V3 x y 1
{-# INLINE addY #-}

-- | CIE xyY color space
deriving instance Eq e => Eq (Pixel (CIExyY i) e)

-- | CIE xyY color space
deriving instance Ord e => Ord (Pixel (CIExyY i) e)

-- | CIE xyY color space
deriving instance Functor (Pixel (CIExyY i))

-- | CIE xyY color space
deriving instance Applicative (Pixel (CIExyY i))

-- | CIE xyY color space
deriving instance Foldable (Pixel (CIExyY i))

-- | CIE xyY color space
deriving instance Traversable (Pixel (CIExyY i))

-- | CIE xyY color space
deriving instance Storable e => Storable (Pixel (CIExyY i) e)

-- | CIE xyY color space
instance (Typeable i, Typeable k, Elevator e) => Show (Pixel (CIExyY (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | CIE xyY color space
instance (Typeable i, Typeable k, Elevator e) => ColorModel (CIExyY (i :: k)) e where
  type Components (CIExyY i) e = (e, e)
  toComponents (CIExyY (V2 x y)) = (x, y)
  {-# INLINE toComponents #-}
  fromComponents (x, y) = CIExyY (V2 x y)
  {-# INLINE fromComponents #-}

-- | CIE xyY color space
instance (Illuminant i, Typeable k, Elevator e) => ColorSpace (CIExyY (i :: k)) i e where
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
