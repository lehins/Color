{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.ColorModel.Elevator
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.Elevator (
  Elevator(..)
  , clamp01
  ) where

import qualified Data.Complex as C
import Data.Int
import Data.Typeable
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import GHC.Float


-- | A class with a set of functions that allow for changing precision by shrinking and
-- streatching the values.
class (Show e, Eq e, Num e, Typeable e, Unbox e, Storable e) => Elevator e where
  maxValue :: e

  minValue :: e

  -- | Values are scaled to @[0, 255]@ range.
  toWord8 :: e -> Word8

  -- | Values are scaled to @[0, 65535]@ range.
  toWord16 :: e -> Word16

  -- | Values are scaled to @[0, 4294967295]@ range.
  toWord32 :: e -> Word32

  -- | Values are scaled to @[0, 18446744073709551615]@ range.
  toWord64 :: e -> Word64

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toRealFloat :: (Elevator a, RealFloat a) => e -> a

  -- | Values are scaled from @[0.0, 1.0]@ range.
  fromRealFloat :: (Elevator a, RealFloat a) => a -> e

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toFloat :: e -> Float
  toFloat = toRealFloat

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toDouble :: e -> Double
  toDouble = toRealFloat

  -- | Values are scaled from @[0.0, 1.0]@ range.
  fromDouble :: Double -> e
  fromDouble = fromRealFloat


-- | Lower the precision
dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b))
{-# INLINE dropDown #-}

-- | Increase the precision
raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}

-- | Convert to fractional with value less than or equal to 1.
squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


-- | Values between @[0, 255]]@
instance Elevator Word8 where
  maxValue = maxBound
  minValue = minBound
  toWord8 = id
  {-# INLINE toWord8 #-}
  toWord16 = raiseUp
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord8
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = toWord8
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 65535]]@
instance Elevator Word16 where
  maxValue = maxBound
  minValue = minBound
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = id
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord16
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = toWord16
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 4294967295]@
instance Elevator Word32 where
  maxValue = maxBound
  minValue = minBound
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = id
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord32
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = toWord32
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 18446744073709551615]@
instance Elevator Word64 where
  maxValue = maxBound
  minValue = minBound
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = id
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord64
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = toWord64
  {-# INLINE fromRealFloat #-}

-- | Values between @[0, 18446744073709551615]@ on 64bit
instance Elevator Word where
  maxValue = maxBound
  minValue = minBound
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}

-- | Values between @[0, 127]@
instance Elevator Int8 where
  maxValue = maxBound
  minValue = 0
  toWord8 = fromIntegral . max 0
  {-# INLINE toWord8 #-}
  toWord16 = raiseUp . max 0
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp . max 0
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . max 0
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 32767]@
instance Elevator Int16 where
  maxValue = maxBound
  minValue = 0
  toWord8 = dropDown . max 0
  {-# INLINE toWord8 #-}
  toWord16 = fromIntegral . max 0
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp . max 0
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . max 0
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 2147483647]@
instance Elevator Int32 where
  maxValue = maxBound
  minValue = 0
  toWord8 = dropDown . max 0
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . max 0
  {-# INLINE toWord16 #-}
  toWord32 = fromIntegral . max 0
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . max 0
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 9223372036854775807]@
instance Elevator Int64 where
  maxValue = maxBound
  minValue = 0
  toWord8 = dropDown . max 0
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . max 0
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . max 0
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral . max 0
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}


-- | Values between @[0, 9223372036854775807]@ on 64bit
instance Elevator Int where
  maxValue = maxBound
  minValue = 0
  toWord8 = dropDown . max 0
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . max 0
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . max 0
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral . max 0
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch . clamp01
  {-# INLINE fromRealFloat #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Float where
  maxValue = 1
  minValue = 0
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = id
  {-# INLINE toFloat #-}
  toDouble = float2Double
  {-# INLINE toDouble #-}
  fromDouble = toFloat
  {-# INLINE fromDouble #-}
  toRealFloat = uncurry encodeFloat . decodeFloat
  {-# INLINE toRealFloat #-}
  fromRealFloat = uncurry encodeFloat . decodeFloat
  {-# INLINE fromRealFloat #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Double where
  maxValue = 1
  minValue = 0
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = double2Float
  {-# INLINE toFloat #-}
  toDouble = id
  {-# INLINE toDouble #-}
  fromDouble = id
  {-# INLINE fromDouble #-}
  toRealFloat = uncurry encodeFloat . decodeFloat
  {-# INLINE toRealFloat #-}
  fromRealFloat = uncurry encodeFloat . decodeFloat
  {-# INLINE fromRealFloat #-}

{-# RULES
"toRealFloat   :: Double -> Double / Float -> Float" toRealFloat = id
"toRealFloat   :: Double -> Float"                   toRealFloat = double2Float
"toRealFloat   :: Float -> Double"                   toRealFloat = float2Double
"fromRealFloat :: Double -> Double / Float -> Float" fromRealFloat = id
"fromRealFloat :: Double -> Float"                   fromRealFloat = double2Float
"fromRealFloat :: Float -> Double"                   fromRealFloat = float2Double
 #-}


-- | Discards imaginary part and changes precision of real part.
instance (Num e, Elevator e, RealFloat e) => Elevator (C.Complex e) where
  maxValue = maxValue C.:+ maxValue
  minValue = minValue C.:+ minValue
  toWord8 = toWord8 . C.realPart
  {-# INLINE toWord8 #-}
  toWord16 = toWord16 . C.realPart
  {-# INLINE toWord16 #-}
  toWord32 = toWord32 . C.realPart
  {-# INLINE toWord32 #-}
  toWord64 = toWord64 . C.realPart
  {-# INLINE toWord64 #-}
  toFloat = toFloat . C.realPart
  {-# INLINE toFloat #-}
  toDouble = toDouble . C.realPart
  {-# INLINE toDouble #-}
  fromDouble = (C.:+ 0) . fromDouble
  {-# INLINE fromDouble #-}
  toRealFloat = toRealFloat . C.realPart
  {-# INLINE toRealFloat #-}
  fromRealFloat = (C.:+ 0) . fromRealFloat
  {-# INLINE fromRealFloat #-}
