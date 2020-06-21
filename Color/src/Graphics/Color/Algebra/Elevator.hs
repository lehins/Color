{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Color.Algebra.Elevator
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Algebra.Elevator
  ( Elevator(..)
  , module Data.Word
  , clamp01
  ) where

import Data.Complex
import Data.Int
import Data.Typeable
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import GHC.Float
import Text.Printf

infixl 7 //

defFieldFormat :: FieldFormat
defFieldFormat = FieldFormat Nothing Nothing Nothing Nothing False "" 'v'

-- | A class with a set of functions that allow for changing precision by shrinking and
-- streatching the values.
class (Show e, Eq e, Num e, Typeable e, Unbox e, Storable e) => Elevator e where
  maxValue :: e

  minValue :: e

  fieldFormat :: e -> FieldFormat
  fieldFormat _ = defFieldFormat

  -- | This is a pretty printer for the value.
  toShowS :: e -> ShowS
  default toShowS :: PrintfArg e => e -> ShowS
  toShowS e = formatArg e (fieldFormat e)

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

  -- | Division that works for integral types as well as floating points. May throw an exception.
  (//) :: e -> e -> e


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
stretch :: forall a b. (RealFloat a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}

-- | Clamp a value to @[0, 1]@ range.
clamp01 :: RealFloat a => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


float2Word32 :: Float -> Word32
float2Word32 d'
  | d' <= 0 = 0
  | d > 4.294967e9 = maxBound
  | otherwise = round d
  where
    d = maxWord32 * d'
{-# INLINE float2Word32 #-}

-- | Same as:
-- >>> fromIntegral (maxBound :: Word32) :: Float
-- 4.2949673e9
--
maxWord32 :: Float
maxWord32 = F# 4.2949673e9#
{-# INLINE maxWord32 #-}

double2Word64 :: Double -> Word64
double2Word64 d'
  | d' <= 0 = 0
  | d > 1.844674407370955e19 = maxBound
  | otherwise = round d
  where
    d = maxWord64 * d'
{-# INLINE double2Word64 #-}

-- | Differs from `fromIntegral` due to: [GHC #17782](https://gitlab.haskell.org/ghc/ghc/issues/17782)
--
-- >>> fromIntegral (maxBound :: Word64) :: Double
-- 1.844674407370955e19
--
maxWord64 :: Double
maxWord64 = D# 1.8446744073709552e19##
{-# INLINE maxWord64 #-}

{-# RULES
"fromRealFloat :: Double -> Word" fromRealFloat = fromDouble :: Double -> Word
"fromRealFloat :: Double -> Word64" fromRealFloat = fromDouble :: Double -> Word64
"fromRealFloat :: Float -> Word32" fromRealFloat = float2Word32
 #-}


-- | Values between @[0, 255]]@
instance Elevator Word8 where
  maxValue = maxBound
  minValue = minBound
  fieldFormat _ = defFieldFormat {fmtWidth = Just 3, fmtChar = 'd'}
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
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 65535]]@
instance Elevator Word16 where
  maxValue = maxBound
  minValue = minBound
  fieldFormat _ = defFieldFormat { fmtWidth = Just 5, fmtChar = 'd'}
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
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 4294967295]@
instance Elevator Word32 where
  maxValue = maxBound
  minValue = minBound
  fieldFormat _ = defFieldFormat { fmtWidth = Just 10, fmtChar = 'd'}
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
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 18446744073709551615]@
instance Elevator Word64 where
  maxValue = maxBound
  minValue = minBound
  fieldFormat _ = defFieldFormat { fmtWidth = Just 20, fmtChar = 'd'}
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
  fromDouble = double2Word64
  {-# INLINE fromDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = toWord64
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}

-- | Values between @[0, 18446744073709551615]@ on 64bit
instance Elevator Word where
  maxValue = maxBound
  minValue = minBound
#if WORD_SIZE_IN_BITS < 64
  fieldFormat _ = defFieldFormat { fmtWidth = Just 10, fmtChar = 'd'}
  toWord64 = dropDown
  {-# INLINE toWord64 #-}
  fromDouble = stretch
  {-# INLINE fromDouble #-}
#else
  fieldFormat _ = defFieldFormat { fmtWidth = Just 20, fmtChar = 'd'}
  toWord64 (W64# w#) = (W# w#)
  {-# INLINE toWord64 #-}
  fromDouble = toWord64 . double2Word64
  {-# INLINE fromDouble #-}
#endif
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  toRealFloat = squashTo1
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}

-- | Values between @[0, 127]@
instance Elevator Int8 where
  maxValue = maxBound
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 3, fmtChar = 'd'}
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
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 32767]@
instance Elevator Int16 where
  maxValue = maxBound
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 5, fmtChar = 'd'}
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
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 2147483647]@
instance Elevator Int32 where
  maxValue = maxBound
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 10, fmtChar = 'd'}
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
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 9223372036854775807]@
instance Elevator Int64 where
  maxValue = maxBound
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 19, fmtChar = 'd'}
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
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0, 9223372036854775807]@ on 64bit
instance Elevator Int where
  maxValue = maxBound
  minValue = 0
#if WORD_SIZE_IN_BITS < 64
  fieldFormat _ = defFieldFormat { fmtWidth = Just 10, fmtChar = 'd'}
  toWord64 = dropDown . max 0
  {-# INLINE toWord64 #-}
#else
  fieldFormat _ = defFieldFormat { fmtWidth = Just 19, fmtChar = 'd'}
  toWord64 = fromIntegral . max 0
  {-# INLINE toWord64 #-}
#endif
  toWord8 = dropDown . max 0
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . max 0
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . max 0
  {-# INLINE toWord32 #-}
  toFloat = squashTo1 . max 0
  {-# INLINE toFloat #-}
  toRealFloat = squashTo1 . max 0
  {-# INLINE toRealFloat #-}
  fromRealFloat = stretch
  {-# INLINE fromRealFloat #-}
  (//) = div
  {-# INLINE (//) #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Float where
  maxValue = 1
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 11, fmtPrecision = Just 8, fmtChar = 'f'}
  toWord8 = stretch
  {-# INLINE toWord8 #-}
  toWord16 = stretch
  {-# INLINE toWord16 #-}
  toWord32 = float2Word32
  {-# INLINE toWord32 #-}
  toWord64 = stretch
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
  (//) = (/)
  {-# INLINE (//) #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Double where
  maxValue = 1
  minValue = 0
  fieldFormat _ = defFieldFormat { fmtWidth = Just 19, fmtPrecision = Just 16, fmtChar = 'f' }
  toWord8 = stretch
  {-# INLINE toWord8 #-}
  toWord16 = stretch
  {-# INLINE toWord16 #-}
  toWord32 = stretch
  {-# INLINE toWord32 #-}
  toWord64 = double2Word64
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
  (//) = (/)
  {-# INLINE (//) #-}

{-# RULES
"toRealFloat   :: Double -> Double / Float -> Float" toRealFloat = id
"toRealFloat   :: Double -> Float"                   toRealFloat = double2Float
"toRealFloat   :: Float -> Double"                   toRealFloat = float2Double
"fromRealFloat :: Double -> Double / Float -> Float" fromRealFloat = id
"fromRealFloat :: Double -> Float"                   fromRealFloat = double2Float
"fromRealFloat :: Float -> Double"                   fromRealFloat = float2Double
 #-}



-- | Discards imaginary part and changes precision of real part.
instance (PrintfArg e, Elevator e, RealFloat e) => Elevator (Complex e) where
  maxValue = maxValue :+ maxValue
  minValue = minValue :+ minValue
  toShowS (r :+ i) = toShowS r . formatArg i ((fieldFormat i) {fmtSign = Just SignPlus}) . ('i' :)
  toWord8 = toWord8 . realPart
  {-# INLINE toWord8 #-}
  toWord16 = toWord16 . realPart
  {-# INLINE toWord16 #-}
  toWord32 = toWord32 . realPart
  {-# INLINE toWord32 #-}
  toWord64 = toWord64 . realPart
  {-# INLINE toWord64 #-}
  toFloat = toFloat . realPart
  {-# INLINE toFloat #-}
  toDouble = toDouble . realPart
  {-# INLINE toDouble #-}
  fromDouble = (:+ 0) . fromDouble
  {-# INLINE fromDouble #-}
  toRealFloat = toRealFloat . realPart
  {-# INLINE toRealFloat #-}
  fromRealFloat = (:+ 0) . fromRealFloat
  {-# INLINE fromRealFloat #-}
  (//) = (/)
  {-# INLINE (//) #-}
