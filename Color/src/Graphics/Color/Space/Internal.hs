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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Space.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.Internal
  ( Color(Luminance, XYZ, CIExyY)
  , ColorSpace(..)
  , Chromaticity(..)
  , Primary(.., Primary)
  , xPrimary
  , yPrimary
  , zPrimary
  , primaryXZ
  , primaryTristimulus
  , Illuminant(..)
  , WhitePoint(.., WhitePoint)
  , xWhitePoint
  , yWhitePoint
  , zWhitePoint
  , whitePointXZ
  , whitePointTristimulus
  , CCT(..)
  , Y
  , pattern Y
  , pattern YA
  , XYZ
  , pattern ColorXYZ
  , pattern ColorXYZA
  , CIExyY
  , pattern ColorCIExy
  , pattern ColorCIExyY
  , module GHC.TypeNats
  , module Graphics.Color.Algebra
  , module Graphics.Color.Model.Internal
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.Y as CM
import Graphics.Color.Algebra
import Data.Typeable
import Data.Coerce
import GHC.TypeNats
import Data.Kind

class (Illuminant i, ColorModel (BaseModel cs) e, ColorModel cs e) =>
  ColorSpace cs (i :: k) e | cs -> i where

  type BaseModel cs :: Type

  type BaseSpace cs :: Type
  type BaseSpace cs = cs

  -- | Drop color space down to the base color model
  toBaseModel :: Color cs e -> Color (BaseModel cs) e
  default toBaseModel ::
    Coercible (Color cs e) (Color (BaseModel cs) e) => Color cs e -> Color (BaseModel cs) e
  toBaseModel = coerce

  -- | Promote color model to a color space
  fromBaseModel :: Color (BaseModel cs) e -> Color cs e
  default fromBaseModel ::
    Coercible (Color (BaseModel cs) e) (Color cs e) => Color (BaseModel cs) e -> Color cs e
  fromBaseModel = coerce

  toBaseSpace :: ColorSpace (BaseSpace cs) i e => Color cs e -> Color (BaseSpace cs) e
  fromBaseSpace :: ColorSpace (BaseSpace cs) i e => Color (BaseSpace cs) e -> Color cs e

  -- | Get the relative luminance of a color
  --
  -- @since 0.1.0
  luminance :: (Elevator a, RealFloat a) => Color cs e -> Color (Y i) a

  toColorXYZ :: (Elevator a, RealFloat a) => Color cs e -> Color (XYZ i) a
  default toColorXYZ ::
    (ColorSpace (BaseSpace cs) i e, Elevator a, RealFloat a) => Color cs e -> Color (XYZ i) a
  toColorXYZ = toColorXYZ . toBaseSpace
  {-# INLINE toColorXYZ #-}

  fromColorXYZ :: (Elevator a, RealFloat a) => Color (XYZ i) a -> Color cs e
  default fromColorXYZ ::
    (ColorSpace (BaseSpace cs) i e, Elevator a, RealFloat a) => Color (XYZ i) a -> Color cs e
  fromColorXYZ = fromBaseSpace . fromColorXYZ
  {-# INLINE fromColorXYZ #-}


instance ( ColorSpace cs i e
         , ColorSpace (BaseSpace cs) i e
         , ColorSpace (Alpha (BaseSpace cs)) i e
         , cs ~ Opaque (Alpha cs)
         , Alpha cs ~ Transparent cs
         , BaseModel cs ~ Opaque (Alpha (BaseModel cs))
         , Alpha (BaseModel cs) ~ Transparent (BaseModel cs)
         ) =>
         ColorSpace (Alpha cs) i e where
  type BaseModel (Alpha cs) = Alpha (BaseModel cs)
  type BaseSpace (Alpha cs) = Alpha (BaseSpace cs)
  toBaseModel = modifyOpaque toBaseModel
  {-# INLINE toBaseModel #-}
  fromBaseModel = modifyOpaque fromBaseModel
  {-# INLINE fromBaseModel #-}
  toColorXYZ = toColorXYZ . dropAlpha
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = (`addAlpha` maxValue) . fromColorXYZ
  {-# INLINE fromColorXYZ #-}
  luminance = luminance . dropAlpha
  {-# INLINE luminance #-}
  toBaseSpace = modifyOpaque toBaseSpace
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = modifyOpaque fromBaseSpace
  {-# INLINE fromBaseSpace #-}

-- | This is a data type that encodes a data point on the chromaticity diagram
newtype Chromaticity i e =
  Chromaticity (Color (CIExyY i) e)
  deriving (Eq, Show)


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


newtype WhitePoint (i :: k) e =
  WhitePointChromaticity (Chromaticity i e)
  deriving (Eq)

instance (Illuminant i, Elevator e) => Show (WhitePoint (i :: k) e) where
  showsPrec n (WhitePointChromaticity wp)
    | n == 0 = inner
    | otherwise = ('(' :) . inner . (')' :)
    where
      inner = ("WhitePoint (" ++) . shows wp . (')' :)

-- | Constructor for the most common @XYZ@ color space
pattern WhitePoint :: e -> e -> WhitePoint i e
pattern WhitePoint x y <- (coerce -> (V2 x y)) where
  WhitePoint x y = coerce (V2 x y)
{-# COMPLETE WhitePoint #-}

-- | @x@ value of a `WhitePoint`
--
-- @since 0.1.0
xWhitePoint :: WhitePoint i e -> e
xWhitePoint (coerce -> V2 x _) = x
{-# INLINE xWhitePoint #-}

-- | @y@ value of a `WhitePoint`
--
-- @since 0.1.0
yWhitePoint :: WhitePoint i e -> e
yWhitePoint (coerce -> V2 _ y) = y
{-# INLINE yWhitePoint #-}

-- | Compute @z@ value of a `WhitePoint`: @z = 1 - x - y@
--
-- @since 0.1.0
zWhitePoint :: Num e => WhitePoint i e -> e
zWhitePoint wp = 1 - xWhitePoint wp - yWhitePoint wp
{-# INLINE zWhitePoint #-}

-- | Compute a normalized @XYZ@ tristimulus of a white point, where @Y = 1@
--
-- @since 0.1.0
whitePointTristimulus ::
     forall i e. (Illuminant i, RealFloat e, Elevator e)
  => Color (XYZ i) e
whitePointTristimulus = toColorXYZ (coerce (whitePoint :: WhitePoint i e) :: Color (CIExyY i) e)
{-# INLINE whitePointTristimulus #-}


-- | Compute @XYZ@ tristimulus of a white point.
--
-- @since 0.1.0
whitePointXZ ::
     Fractional e
  => e
     -- ^ @Y@ value, which is usually set to @1@
  -> WhitePoint i e
     -- ^ White point that specifies @x@ and @y@
  -> Color (XYZ i) e
whitePointXZ vY (coerce -> V2 x y) = ColorXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE whitePointXZ #-}

-------------
-- Primary --
-------------

newtype Primary (i :: k) e = PrimaryChromaticity (Chromaticity i e)
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




-- | Compute normalized `XYZ` tristimulus of a `Primary`, where @Y = 1@
--
-- @since 0.1.0
primaryTristimulus ::
     forall i e. (Illuminant i, RealFloat e, Elevator e)
  => Primary i e
     -- ^ Primary that specifies @x@ and @y@
  -> Color (XYZ i) e
primaryTristimulus xy = toColorXYZ (coerce xy :: Color (CIExyY i) e)
{-# INLINE primaryTristimulus #-}

-- | Compute `XYZ` tristimulus of a `Primary`.
--
-- @since 0.1.0
primaryXZ ::
     Fractional e =>
     e
     -- ^ @Y@ value, which is usually set to @1@
  -> Primary i e
     -- ^ Primary that specifies @x@ and @y@
  -> Color (XYZ i) e
primaryXZ vY (Primary x y) = ColorXYZ (vYy * x) vY (vYy * (1 - x - y))
  where !vYy = vY / y
{-# INLINE primaryXZ #-}


-----------
--- XYZ ---
-----------

-- | The original color space CIE 1931 XYZ color space
data XYZ i

-- | CIE1931 `XYZ` color space
newtype instance Color (XYZ i) e = XYZ (V3 e)

-- | Constructor for the most common @XYZ@ color space
pattern ColorXYZ :: e -> e -> e -> Color (XYZ i) e
pattern ColorXYZ x y z = XYZ (V3 x y z)
{-# COMPLETE ColorXYZ #-}

-- | Constructor for @XYZ@ with alpha channel.
pattern ColorXYZA :: e -> e -> e -> e -> Color (Alpha (XYZ i)) e
pattern ColorXYZA x y z a = Alpha (XYZ (V3 x y z)) a
{-# COMPLETE ColorXYZA #-}


-- | CIE1931 `XYZ` color space
deriving instance Eq e => Eq (Color (XYZ i) e)
-- | CIE1931 `XYZ` color space
deriving instance Ord e => Ord (Color (XYZ i) e)
-- | CIE1931 `XYZ` color space
deriving instance Functor (Color (XYZ i))
-- | CIE1931 `XYZ` color space
deriving instance Applicative (Color (XYZ i))
-- | CIE1931 `XYZ` color space
deriving instance Foldable (Color (XYZ i))
-- | CIE1931 `XYZ` color space
deriving instance Traversable (Color (XYZ i))
-- | CIE1931 `XYZ` color space
deriving instance Storable e => Storable (Color (XYZ i) e)

-- | CIE1931 `XYZ` color space
instance (Illuminant i, Elevator e) => Show (Color (XYZ (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | CIE1931 `XYZ` color space
instance (Illuminant i, Elevator e) => ColorModel (XYZ (i :: k)) e where
  type Components (XYZ i) e = (e, e, e)
  toComponents (ColorXYZ x y z) = (x, y, z)
  {-# INLINE toComponents #-}
  fromComponents (x, y, z) = ColorXYZ x y z
  {-# INLINE fromComponents #-}

-- | CIE1931 `XYZ` color space
instance (Illuminant i, Elevator e) => ColorSpace (XYZ i) i e where
  type BaseModel (XYZ i) = XYZ i
  toBaseModel = id
  fromBaseModel = id
  toBaseSpace = id
  fromBaseSpace = id
  luminance (ColorXYZ _ y _) = Y (toRealFloat y)
  {-# INLINE luminance #-}
  toColorXYZ (ColorXYZ x y z) = ColorXYZ (toRealFloat x) (toRealFloat y) (toRealFloat z)
  {-# INLINE toColorXYZ #-}
  fromColorXYZ (ColorXYZ x y z) = ColorXYZ (fromRealFloat x) (fromRealFloat y) (fromRealFloat z)
  {-# INLINE fromColorXYZ #-}

{-# RULES
"toColorXYZ   :: Color (XYZ i) a -> Color (XYZ i) a"   toColorXYZ = id
"fromColorXYZ :: Color (XYZ i) a -> Color (XYZ i) a" fromColorXYZ = id
 #-}


---------------
--- CIE xyY ---
---------------

-- | Alternative representation of the CIE 1931 XYZ color space
data CIExyY (i :: k)

-- | CIE1931 `CIExyY` color space
newtype instance Color (CIExyY i) e = CIExyY (V2 e)

-- | Constructor @CIE xyY@ color space. It only requires @x@ and @y@, then @Y@ part will
-- always be equal to 1.
pattern ColorCIExy :: e -> e -> Color (CIExyY i) e
pattern ColorCIExy x y = CIExyY (V2 x y)
{-# COMPLETE ColorCIExy #-}

-- | Patttern match on the @CIE xyY@, 3rd argument @Y@ is always set to @1@
pattern ColorCIExyY :: Num e => e -> e -> e -> Color (CIExyY i) e
pattern ColorCIExyY x y y' <- (addY -> V3 x y y')
{-# COMPLETE ColorCIExyY #-}

addY :: Num e => Color (CIExyY i) e -> V3 e
addY (CIExyY (V2 x y)) = V3 x y 1
{-# INLINE addY #-}

-- | CIE xyY color space
deriving instance Eq e => Eq (Color (CIExyY i) e)
-- | CIE xyY color space
deriving instance Ord e => Ord (Color (CIExyY i) e)
-- | CIE xyY color space
deriving instance Functor (Color (CIExyY i))
-- | CIE xyY color space
deriving instance Applicative (Color (CIExyY i))
-- | CIE xyY color space
deriving instance Foldable (Color (CIExyY i))
-- | CIE xyY color space
deriving instance Traversable (Color (CIExyY i))
-- | CIE xyY color space
deriving instance Storable e => Storable (Color (CIExyY i) e)

-- | CIE xyY color space
instance (Illuminant i, Elevator e) => Show (Color (CIExyY (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | CIE xyY color space
instance (Illuminant i, Elevator e) => ColorModel (CIExyY (i :: k)) e where
  type Components (CIExyY i) e = (e, e)
  toComponents (CIExyY (V2 x y)) = (x, y)
  {-# INLINE toComponents #-}
  fromComponents (x, y) = CIExyY (V2 x y)
  {-# INLINE fromComponents #-}
  showsColorModelName _ = showsType (Proxy :: Proxy (CIExyY i))

-- | CIE xyY color space
instance (Illuminant i, Elevator e) => ColorSpace (CIExyY (i :: k)) i e where
  type BaseModel (CIExyY i) = CIExyY i
  toBaseModel = id
  fromBaseModel = id
  toBaseSpace = id
  fromBaseSpace = id
  luminance _ = Y 1
  {-# INLINE luminance #-}
  toColorXYZ xy = ColorXYZ (x / y) 1 ((1 - x - y) / y)
    where ColorCIExy x y = toRealFloat <$> xy
  {-# INLINE toColorXYZ #-}
  fromColorXYZ xyz = fromRealFloat <$> ColorCIExy (x / s) (y / s)
    where
      ColorXYZ x y z = xyz
      !s = x + y + z
  {-# INLINE fromColorXYZ #-}



-------------
--- Y ---
-------------

-- | [Relative Luminance](https://en.wikipedia.org/wiki/Relative_luminance) of a color
data Y (i :: k)

-- | Luminance `Y`
newtype instance Color (Y i) e = Luminance (CM.Color CM.Y e)

-- | Constructor for @Y@ with alpha channel.
pattern Y :: e -> Color (Y i) e
pattern Y y = Luminance (CM.Y y)
{-# COMPLETE Y #-}

-- | Constructor for @Y@ with alpha channel.
pattern YA :: e -> e -> Color (Alpha (Y i)) e
pattern YA y a = Alpha (Luminance (CM.Y y)) a
{-# COMPLETE YA #-}

-- | `Y` - relative luminance of a color space
deriving instance Eq e => Eq (Color (Y i) e)
-- | `Y` - relative luminance of a color space
deriving instance Ord e => Ord (Color (Y i) e)
-- | `Y` - relative luminance of a color space
deriving instance Functor (Color (Y i))
-- | `Y` - relative luminance of a color space
deriving instance Applicative (Color (Y i))
-- | `Y` - relative luminance of a color space
deriving instance Foldable (Color (Y i))
-- | `Y` - relative luminance of a color space
deriving instance Traversable (Color (Y i))
-- | `Y` - relative luminance of a color space
deriving instance Storable e => Storable (Color (Y i) e)


-- | `Y` - relative luminance of a color space
instance (Illuminant i, Elevator e) => Show (Color (Y i) e) where
  showsPrec _ = showsColorModel

-- | `Y` - relative luminance of a color space
instance (Illuminant i, Elevator e) => ColorModel (Y i) e where
  type Components (Y i) e = e
  toComponents = coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce
  {-# INLINE fromComponents #-}


-- | CIE1931 `XYZ` color space
instance (Illuminant i, Elevator e) => ColorSpace (Y i) i e where
  type BaseModel (Y i) = CM.Y
  toBaseSpace = id
  fromBaseSpace = id
  luminance = fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ (Y y) = ColorXYZ 0 (toRealFloat y) 0
  {-# INLINE toColorXYZ #-}
  fromColorXYZ (ColorXYZ _ y _) = Y (fromRealFloat y)
  {-# INLINE fromColorXYZ #-}

{-# RULES
"luminance   :: RealFloat a => Color Y a -> Color Y a" luminance = id
 #-}
