{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Color.Algebra
-- Copyright   : (c) Alexey Kuleshevich 2019-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Algebra
  ( -- * 2D
    V2(..)
    -- * 3D
  , V3(..)
  , showV3
  , dotProduct
  , M3x3(..)
  , showM3x3
  , detM3x3
  , invertM3x3
  , multM3x3byV3
  , multM3x3byM3x3
  , multM3x3byV3d
  , transposeM3x3
  , module Graphics.Color.Algebra.Elevator
  -- * Helpers
  , showsType
  , asProxy
  ) where

import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Graphics.Color.Algebra.Elevator
#if MIN_VERSION_base(4,10,0) && !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif


--------
-- V2 --
--------

-- | A 2D vector with @x@ and @y@ coordinates.
data V2 a = V2 !a !a
  deriving (Eq, Ord)

instance Elevator a => Show (V2 a) where
  showsPrec _ (V2 x y) =
    ('[' :) . toShowS x . (',' :) . toShowS y . (" ]" ++)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)
  {-# INLINE fmap #-}


zipWithV2 :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
zipWithV2 f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)
{-# INLINE zipWithV2 #-}

instance Applicative V2 where
  pure x = V2 x x
  {-# INLINE pure #-}
  (<*>) (V2 fx1 fy1) (V2 x2 y2) = V2 (fx1 x2) (fy1 y2)
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = zipWithV2
  {-# INLINE liftA2 #-}
#endif

instance Foldable V2 where
  foldr f acc (V2 x y) = f x (f y acc)
  {-# INLINE foldr #-}

instance Traversable V2 where
  traverse f (V2 x y) = V2 <$> f x <*> f y
  {-# INLINE traverse #-}

instance Num a => Num (V2 a) where
  (+)         = zipWithV2 (+)
  {-# INLINE (+) #-}
  (-)         = zipWithV2 (-)
  {-# INLINE (-) #-}
  (*)         = zipWithV2 (*)
  {-# INLINE (*) #-}
  abs         = fmap abs
  {-# INLINE abs #-}
  signum      = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional a => Fractional (V2 a) where
  (/)          = zipWithV2 (/)
  {-# INLINE (/) #-}
  recip        = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance Floating a => Floating (V2 a) where
  pi      = pure pi
  {-# INLINE pi #-}
  exp     = fmap exp
  {-# INLINE exp #-}
  log     = fmap log
  {-# INLINE log #-}
  sin     = fmap sin
  {-# INLINE sin #-}
  cos     = fmap cos
  {-# INLINE cos #-}
  asin    = fmap asin
  {-# INLINE asin #-}
  atan    = fmap atan
  {-# INLINE atan #-}
  acos    = fmap acos
  {-# INLINE acos #-}
  sinh    = fmap sinh
  {-# INLINE sinh #-}
  cosh    = fmap cosh
  {-# INLINE cosh #-}
  asinh   = fmap asinh
  {-# INLINE asinh #-}
  atanh   = fmap atanh
  {-# INLINE atanh #-}
  acosh   = fmap acosh
  {-# INLINE acosh #-}


instance Storable e => Storable (V2 e) where
  sizeOf _ = 2 * sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek p =
    let q = castPtr p
     in V2 <$> peek q <*> peekElemOff q 1
  {-# INLINE peek #-}
  poke p (V2 v0 v1) =
    let q = castPtr p
     in poke q v0 >> pokeElemOff q 1 v1
  {-# INLINE poke #-}


--------
-- V3 --
--------

-- | A 3D vector with @x@, @y@ and @z@ coordinates.
data V3 a = V3 !a !a !a
  deriving (Eq, Ord)

instance Elevator a => Show (V3 a) where
  showsPrec _ (V3 x y z) =
    ('[' :) . toShowS x . (',' :) . toShowS y . (',' :) . toShowS z . (" ]" ++)

showV3 :: Show a => V3 a -> String
showV3 (V3 x y z) = concat ["[ ", show x, ", ", show y, ", ", show z, " ]"]

-- | Mulitply a 1x3 vector by a 3x1 vector, i.e. dot product.
--
-- @since 0.1.0
dotProduct :: Num a => V3 a -> V3 a -> a
dotProduct (V3 u0 u1 u2) (V3 v0 v1 v2) = u0 * v0 + u1 * v1 + u2 * v2
{-# INLINE dotProduct #-}


zipWithV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
zipWithV3 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)
{-# INLINE zipWithV3 #-}

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  {-# INLINE fmap #-}

instance Applicative V3 where
  pure x = V3 x x x
  {-# INLINE pure #-}
  (<*>) (V3 fx1 fy1 fz1) (V3 x2 y2 z2) = V3 (fx1 x2) (fy1 y2) (fz1 z2)
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = zipWithV3
  {-# INLINE liftA2 #-}
#endif

instance Foldable V3 where
  foldr f acc (V3 x y z) = f x (f y (f z acc))
  {-# INLINE foldr #-}

instance Traversable V3 where
  traverse f (V3 x y z) = V3 <$> f x <*> f y <*> f z
  {-# INLINE traverse #-}

instance Num a => Num (V3 a) where
  (+)         = zipWithV3 (+)
  {-# INLINE (+) #-}
  (-)         = zipWithV3 (-)
  {-# INLINE (-) #-}
  (*)         = zipWithV3 (*)
  {-# INLINE (*) #-}
  abs         = fmap abs
  {-# INLINE abs #-}
  signum      = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional a => Fractional (V3 a) where
  (/)          = zipWithV3 (/)
  {-# INLINE (/) #-}
  recip        = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance Floating a => Floating (V3 a) where
  pi      = pure pi
  {-# INLINE pi #-}
  exp     = fmap exp
  {-# INLINE exp #-}
  log     = fmap log
  {-# INLINE log #-}
  sin     = fmap sin
  {-# INLINE sin #-}
  cos     = fmap cos
  {-# INLINE cos #-}
  asin    = fmap asin
  {-# INLINE asin #-}
  atan    = fmap atan
  {-# INLINE atan #-}
  acos    = fmap acos
  {-# INLINE acos #-}
  sinh    = fmap sinh
  {-# INLINE sinh #-}
  cosh    = fmap cosh
  {-# INLINE cosh #-}
  asinh   = fmap asinh
  {-# INLINE asinh #-}
  atanh   = fmap atanh
  {-# INLINE atanh #-}
  acosh   = fmap acosh
  {-# INLINE acosh #-}


instance Storable e => Storable (V3 e) where
  sizeOf _ = 3 * sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek p = do
    let q = castPtr p
    v0 <- peek q
    v1 <- peekElemOff q 1
    v2 <- peekElemOff q 2
    return $! V3 v0 v1 v2

  {-# INLINE peek #-}
  poke p (V3 v0 v1 v2) = do
    let q = castPtr p
    poke q v0
    pokeElemOff q 1 v1
    pokeElemOff q 2 v2
  {-# INLINE poke #-}

----------
-- M3x3 --
----------



-- | A 3x3 Matrix
data M3x3 a = M3x3
  { m3x3row0 :: {-# UNPACK #-}!(V3 a)
  , m3x3row1 :: {-# UNPACK #-}!(V3 a)
  , m3x3row2 :: {-# UNPACK #-}!(V3 a)
  } deriving (Eq)

instance Elevator a => Show (M3x3 a) where
  showsPrec _ (M3x3 v0 v1 v2) =
    ("[ " ++) . shows v0 . ("\n, " ++) . shows v1 . ("\n, " ++) . shows v2 . (" ]" ++)


showM3x3 :: Show a => M3x3 a -> String
showM3x3 (M3x3 v0 v1 v2) =
  concat ["[ ", showV3 v0, "\n, ", showV3 v1, "\n, ", showV3 v2, " ]"]


-- | Mulitply a 3x3 matrix by a 3x1 vector, while getting a vector back.
--
-- @since 0.1.0
multM3x3byV3 :: Num a => M3x3 a -> V3 a -> V3 a
multM3x3byV3 (M3x3 (V3 a b c)
                   (V3 d e f)
                   (V3 g h i)) (V3 v0 v1 v2) = V3 (a * v0 + b * v1 + c * v2)
                                                  (d * v0 + e * v1 + f * v2)
                                                  (g * v0 + h * v1 + i * v2)
{-# INLINE multM3x3byV3 #-}


multM3x3byM3x3 :: Num a => M3x3 a -> M3x3 a -> M3x3 a
multM3x3byM3x3 m1 m2 =
  M3x3
  (V3 (a1 * a2 + b1 * d2 + c1 * g2) (a1 * b2 + b1 * e2 + c1 * h2) (a1 * c2 + b1 * f2 + c1 * i2))
  (V3 (d1 * a2 + e1 * d2 + f1 * g2) (d1 * b2 + e1 * e2 + f1 * h2) (d1 * c2 + e1 * f2 + f1 * i2))
  (V3 (g1 * a2 + h1 * d2 + i1 * g2) (g1 * b2 + h1 * e2 + i1 * h2) (g1 * c2 + h1 * f2 + i1 * i2))
  where
    M3x3 (V3 a1 b1 c1)
         (V3 d1 e1 f1)
         (V3 g1 h1 i1) = m1
    M3x3 (V3 a2 b2 c2)
         (V3 d2 e2 f2)
         (V3 g2 h2 i2) = m2
{-# INLINE multM3x3byM3x3 #-}

-- | Multiply a 3x3 matrix by another 3x3 diagonal matrix represented by a 1x3 vector
multM3x3byV3d :: Num a => M3x3 a -> V3 a -> M3x3 a
multM3x3byV3d m1 m2 =
  M3x3
  (V3 (a1 * a2) (b1 * e2) (c1 * i2))
  (V3 (d1 * a2) (e1 * e2) (f1 * i2))
  (V3 (g1 * a2) (h1 * e2) (i1 * i2))
  where
    M3x3 (V3 a1 b1 c1)
         (V3 d1 e1 f1)
         (V3 g1 h1 i1) = m1
    V3 a2 e2 i2 = m2
{-# INLINE multM3x3byV3d #-}


-- | Invert a 3x3 matrix.
--
-- @since 0.1.0
invertM3x3 :: Fractional a => M3x3 a -> M3x3 a
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
detM3x3 :: Num a => M3x3 a -> a
detM3x3 (M3x3 (V3 i00 i01 i02)
              (V3 i10 i11 i12)
              (V3 i20 i21 i22)) = i00 * (i11 * i22 - i12 * i21) +
                                  i01 * (i12 * i20 - i10 * i22) +
                                  i02 * (i10 * i21 - i11 * i20)
{-# INLINE detM3x3 #-}


transposeM3x3 :: M3x3 a -> M3x3 a
transposeM3x3 (M3x3 (V3 i00 i01 i02)
                    (V3 i10 i11 i12)
                    (V3 i20 i21 i22)) = M3x3 (V3 i00 i10 i20)
                                             (V3 i01 i11 i21)
                                             (V3 i02 i12 i22)
{-# INLINE transposeM3x3 #-}



pureM3x3 :: a -> M3x3 a
pureM3x3 x = M3x3 (pure x) (pure x) (pure x)
{-# INLINE pureM3x3 #-}

mapM3x3 :: (a -> a) -> M3x3 a -> M3x3 a
mapM3x3 f (M3x3 v0 v1 v2) = M3x3 (fmap f v0) (fmap f v1) (fmap f v2)
{-# INLINE mapM3x3 #-}

zipWithM3x3 :: (a -> b -> c) -> M3x3 a -> M3x3 b -> M3x3 c
zipWithM3x3 f (M3x3 v10 v11 v12) (M3x3 v20 v21 v22) =
  M3x3 (zipWithV3 f v10 v20) (zipWithV3 f v11 v21) (zipWithV3 f v12 v22)
{-# INLINE zipWithM3x3 #-}


instance Functor M3x3 where
  fmap f (M3x3 v0 v1 v2) = M3x3 (fmap f v0) (fmap f v1) (fmap f v2)
  {-# INLINE fmap #-}

instance Applicative M3x3 where
  pure x = M3x3 (pure x) (pure x) (pure x)
  {-# INLINE pure #-}
  (<*>) (M3x3 fx1 fy1 fz1) (M3x3 x2 y2 z2) = M3x3 (fx1 <*> x2) (fy1 <*> y2) (fz1 <*> z2)
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = zipWithM3x3
  {-# INLINE liftA2 #-}
#endif

instance Foldable M3x3 where
  foldr f acc (M3x3 x y z) = foldr f (foldr f (foldr f acc z) y) x
  {-# INLINE foldr #-}

instance Traversable M3x3 where
  traverse f (M3x3 x y z) = M3x3 <$> traverse f x <*> traverse f y <*> traverse f z
  {-# INLINE traverse #-}


instance Num a => Num (M3x3 a) where
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


instance Fractional a => Fractional (M3x3 a) where
  (/)          = zipWithM3x3 (/)
  {-# INLINE (/) #-}
  recip        = mapM3x3 recip
  {-# INLINE recip #-}
  fromRational = pureM3x3 . fromRational
  {-# INLINE fromRational #-}


instance Floating a => Floating (M3x3 a) where
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



showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep

asProxy :: p -> (Proxy p -> t) -> t
asProxy _ f = f (Proxy :: Proxy a)
