{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.ColorSpace.Algebra
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Algebra
  ( V3(..)
  , toV3
  , fromV3
  , showV3
  , printV3
  , M3x3(..)
  , showM3x3
  , printM3x3
  , detM3x3
  , invertM3x3
  , multM3x3byV3
  , transposeM3x3
  , NPM(..)
  , INPM(..)
  , npmCompute
  , inpmCompute
  , Primary(..)
  , zPrimary
  , primaryXZ
  , primaryXYZ
  , WhitePoint(..)
  , whitePointXZ
  , whitePointXYZ
  , Chromaticity(..)
  ) where

import Text.Printf
import Graphics.ColorSpace.Internal
import Graphics.ColorModel.Elevator
import Data.Coerce

import Debug.Trace

-- | A 3D vector with @x@, @y@ and @z@ components in double floating point precision.
data V3 =
  V3 {-# UNPACK #-}!Double
     {-# UNPACK #-}!Double
     {-# UNPACK #-}!Double
  deriving (Eq)

instance Show V3 where
  show (V3 x y z) = printf "[% .7f,% .7f,% .7f]" x y z

toV3 :: Elevator e => e -> e -> e -> V3
toV3 v0 v1 v2 = V3 (toDouble v0) (toDouble v1) (toDouble v2)
{-# INLINE[1] toV3 #-}

{-# RULES
"toV3 :: Double -> Double -> Double -> V3" toV3 = V3
  #-}

fromV3 :: Elevator e => (e -> e -> e -> a) -> V3 -> a
fromV3 mk (V3 v0 v1 v2) = mk (fromDouble v0) (fromDouble v1) (fromDouble v2)


-- | A 3x3 Matrix
data M3x3 =
  M3x3 {-# UNPACK #-}!V3
       {-# UNPACK #-}!V3
       {-# UNPACK #-}!V3
  deriving (Eq)

instance Show M3x3 where
  showsPrec _ (M3x3 v0 v1 v2) =
    ("[ " ++) . shows v0 . ("\n, " ++) . shows v1 . ("\n, " ++) . shows v2 . (++ " ]")

showV3 :: V3 -> String
showV3 (V3 x y z) = concat ["[ ", show x, ", ", show y, ", ", show z, " ]"]

printV3 :: V3 -> IO ()
printV3 = putStrLn . showV3

showM3x3 :: M3x3 -> String
showM3x3 (M3x3 v0 v1 v2) =
  concat ["[ ", showV3 v0, "\n, ", showV3 v1, "\n, ", showV3 v2, " ]"]

printM3x3 :: M3x3 -> IO ()
printM3x3 = putStrLn . showM3x3

-- | Mulitply a 3x3 matrix by a 3x1 vector, while getting a vector back.
--
-- @since 0.1.0
multM3x3byV3 :: M3x3 -> V3 -> V3
multM3x3byV3 (M3x3 (V3 a b c)
                   (V3 d e f)
                   (V3 g h i)) (V3 v0 v1 v2) = V3 (a * v0 + b * v1 + c * v2)
                                                  (d * v0 + e * v1 + f * v2)
                                                  (g * v0 + h * v1 + i * v2)
{-# INLINE multM3x3byV3 #-}


-- | Invert a 3x3 matrix.
--
-- @since 0.1.0
invertM3x3 :: M3x3 -> M3x3
invertM3x3 (M3x3 (V3 a b c)
                 (V3 d e f)
                 (V3 g h i)) =
  M3x3 (V3 (a' / det) (d' / det) (g' / det))
       (V3 (b' / det) (e' / det) (h' / det))
       (V3 (c' / det) (f' / det) (i' / det))
  where
    !a' =   e*i - f*h
    !b' = -(d*i - f*g)
    !c' =   d*h - e*g
    !d' = -(b*i - c*h)
    !e' =   a*i - c*g
    !f' = -(a*h - b*g)
    !g' =   b*f - c*e
    !h' = -(a*f - c*d)
    !i' =   a*e - b*d
    !det = a*a' + b*b' + c*c'
{-# INLINE invertM3x3 #-}


-- | Compute a determinant of a 3x3 matrix.
--
-- @since 0.1.0
detM3x3 :: M3x3 -> Double
detM3x3 (M3x3 (V3 i00 i01 i02)
              (V3 i10 i11 i12)
              (V3 i20 i21 i22)) = i00 * (i11 * i22 - i12 * i21) +
                                  i01 * (i12 * i20 - i10 * i22) +
                                  i02 * (i10 * i21 - i11 * i20)
{-# INLINE detM3x3 #-}


transposeM3x3 :: M3x3 -> M3x3
transposeM3x3 (M3x3 (V3 i00 i01 i02)
                    (V3 i10 i11 i12)
                    (V3 i20 i21 i22)) = M3x3 (V3 i00 i10 i20)
                                             (V3 i01 i11 i21)
                                             (V3 i02 i12 i22)
{-# INLINE transposeM3x3 #-}


pureV3 :: Double -> V3
pureV3 x = V3 x x x
{-# INLINE pureV3 #-}

mapV3 :: (Double -> Double) -> V3 -> V3
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)
{-# INLINE mapV3 #-}

zipWithV3 :: (Double -> Double -> Double) -> V3 -> V3 -> V3
zipWithV3 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)
{-# INLINE zipWithV3 #-}

instance Num V3 where
  (+)         = zipWithV3 (+)
  {-# INLINE (+) #-}
  (-)         = zipWithV3 (-)
  {-# INLINE (-) #-}
  (*)         = zipWithV3 (*)
  {-# INLINE (*) #-}
  abs         = mapV3 abs
  {-# INLINE abs #-}
  signum      = mapV3 signum
  {-# INLINE signum #-}
  fromInteger = pureV3 . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional V3 where
  (/)          = zipWithV3 (/)
  {-# INLINE (/) #-}
  recip        = mapV3 recip
  {-# INLINE recip #-}
  fromRational = pureV3 . fromRational
  {-# INLINE fromRational #-}


instance Floating V3 where
  pi      = pureV3 pi
  {-# INLINE pi #-}
  exp     = mapV3 exp
  {-# INLINE exp #-}
  log     = mapV3 log
  {-# INLINE log #-}
  sin     = mapV3 sin
  {-# INLINE sin #-}
  cos     = mapV3 cos
  {-# INLINE cos #-}
  asin    = mapV3 asin
  {-# INLINE asin #-}
  atan    = mapV3 atan
  {-# INLINE atan #-}
  acos    = mapV3 acos
  {-# INLINE acos #-}
  sinh    = mapV3 sinh
  {-# INLINE sinh #-}
  cosh    = mapV3 cosh
  {-# INLINE cosh #-}
  asinh   = mapV3 asinh
  {-# INLINE asinh #-}
  atanh   = mapV3 atanh
  {-# INLINE atanh #-}
  acosh   = mapV3 acosh
  {-# INLINE acosh #-}

pureM3x3 :: Double -> M3x3
pureM3x3 x = M3x3 (pureV3 x) (pureV3 x) (pureV3 x)
{-# INLINE pureM3x3 #-}

mapM3x3 :: (Double -> Double) -> M3x3 -> M3x3
mapM3x3 f (M3x3 v0 v1 v2) = M3x3 (mapV3 f v0) (mapV3 f v1) (mapV3 f v2)
{-# INLINE mapM3x3 #-}

zipWithM3x3 :: (Double -> Double -> Double) -> M3x3 -> M3x3 -> M3x3
zipWithM3x3 f (M3x3 v10 v11 v12) (M3x3 v20 v21 v22) =
  M3x3 (zipWithV3 f v10 v20) (zipWithV3 f v11 v21) (zipWithV3 f v12 v22)
{-# INLINE zipWithM3x3 #-}


instance Num M3x3 where
  (+)         = zipWithM3x3 (+)
  {-# INLINE (+) #-}
  (-)         = zipWithM3x3 (-)
  {-# INLINE (-) #-}
  (*)         = zipWithM3x3 (*)
  {-# INLINE (*) #-}
  abs         = mapM3x3 abs
  {-# INLINE abs #-}
  signum      = mapM3x3 signum
  {-# INLINE signum #-}
  fromInteger = pureM3x3 . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional M3x3 where
  (/)          = zipWithM3x3 (/)
  {-# INLINE (/) #-}
  recip        = mapM3x3 recip
  {-# INLINE recip #-}
  fromRational = pureM3x3 . fromRational
  {-# INLINE fromRational #-}


instance Floating M3x3 where
  pi      = pureM3x3 pi
  {-# INLINE pi #-}
  exp     = mapM3x3 exp
  {-# INLINE exp #-}
  log     = mapM3x3 log
  {-# INLINE log #-}
  sin     = mapM3x3 sin
  {-# INLINE sin #-}
  cos     = mapM3x3 cos
  {-# INLINE cos #-}
  asin    = mapM3x3 asin
  {-# INLINE asin #-}
  atan    = mapM3x3 atan
  {-# INLINE atan #-}
  acos    = mapM3x3 acos
  {-# INLINE acos #-}
  sinh    = mapM3x3 sinh
  {-# INLINE sinh #-}
  cosh    = mapM3x3 cosh
  {-# INLINE cosh #-}
  asinh   = mapM3x3 asinh
  {-# INLINE asinh #-}
  atanh   = mapM3x3 atanh
  {-# INLINE atanh #-}
  acosh   = mapM3x3 acosh
  {-# INLINE acosh #-}


-- | Compute @z = 1 - x - y@ of a `Primary`.
zPrimary :: Primary -> Double
zPrimary p = 1 - xPrimary p - yPrimary p
{-# INLINE zPrimary #-}


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



newtype NPM cs (i :: k) = NPM
  { unNPM :: M3x3
  } deriving (Eq, Show)

newtype INPM cs (i :: k) = INPM
  { unINPM :: M3x3
  } deriving (Eq, Show)


npmCompute :: forall cs i . Illuminant i => Chromaticity i -> NPM cs i
npmCompute (Chromaticity r g b) = trace "shit" $ NPM (primaries' * M3x3 coeff coeff coeff)
  where
    -- transposed matrix with xyz primaries
    primaries' = M3x3 (V3 (xPrimary r) (xPrimary g) (xPrimary b))
                      (V3 (yPrimary r) (yPrimary g) (yPrimary b))
                      (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    coeff = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ (whitePoint :: WhitePoint i)

inpmCompute :: forall cs i . Illuminant i => Chromaticity i -> INPM cs i
inpmCompute = coerce . invertM3x3 . coerce . npmCompute
