{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Algebra.Binary
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Algebra.Binary
  ( Bit
  , zero
  , one
  , toBool
  , fromBool
  , toNum
  , fromNum
  ) where

import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Foreign.Storable
import Graphics.Color.Algebra.Elevator
import Prelude hiding (map)
import Data.Coerce

-- | Under the hood, binary pixels are backed by `Word8`, but can only take
-- values of @0@ or @1@. Use `zero`\/`one` to construct a bit and `on`\/`off` to
-- construct a binary pixel.
newtype Bit = Bit Word8 deriving (Ord, Eq, Storable)


instance Show Bit where
  show (Bit 0) = "0"
  show _       = "1"

cf :: (Word8 -> Word8) -> Bit -> Bit
cf = coerce

cf2 :: (Word8 -> Word8 -> Word8) -> Bit -> Bit -> Bit
cf2 = coerce

instance Bits Bit where
  (.&.) = cf2 (.&.)
  {-# INLINE (.&.) #-}
  (.|.) = cf2 (.|.)
  {-# INLINE (.|.) #-}
  xor = cf2 xor
  {-# INLINE xor #-}
  complement = cf complement
  {-# INLINE complement #-}
  shift !b 0 = b
  shift  _ _ = Bit 0
  {-# INLINE shift #-}
  rotate !b _ = b
  {-# INLINE rotate #-}
  zeroBits = zero
  {-# INLINE zeroBits #-}
  bit 0 = one
  bit _ = zero
  {-# INLINE bit #-}
  testBit (Bit b) 0 = b /= 0
  testBit _       _ = False
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Just 1
  {-# INLINE bitSizeMaybe #-}
  bitSize _ = 1
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  popCount (Bit 0) = 0
  popCount _       = 1
  {-# INLINE popCount #-}


-- | Convert `Bit` to `Bool`
--
-- @since 0.1.0
toBool :: Bit -> Bool
toBool (Bit 0) = False
toBool _       = True
{-# INLINE toBool #-}

-- | Convert `Bool` to `Bit`
--
-- @since 0.1.0
fromBool :: Bool -> Bit
fromBool False = zero
fromBool True  = one
{-# INLINE fromBool #-}

-- | Convert a bit to a number.
--
-- @since 0.1.0
toNum :: Num a => Bit -> a
toNum (Bit 0) = 0
toNum _       = 1
{-# INLINE toNum #-}

-- | Convert a number to a bit. Any non-zero number corresponds to @1@.
--
-- @since 0.1.0
fromNum :: (Eq a, Num a) => a -> Bit
fromNum 0 = zero
fromNum _ = one
{-# INLINE fromNum #-}


zero :: Bit
zero = coerce (0x00 :: Word8)
{-# INLINE zero #-}

one :: Bit
one = coerce (0xff :: Word8)
{-# INLINE one #-}


-- | Values: @0@ and @1@
instance Elevator Bit where
  minValue = Bit 0x00
  {-# INLINE minValue #-}
  maxValue = Bit 0xff
  {-# INLINE maxValue #-}
  toShowS (Bit 0) = ('0':)
  toShowS _       = ('1':)
  toWord8 = coerce
  {-# INLINE toWord8 #-}
  toWord16 (Bit 0) = 0
  toWord16 _       = maxBound
  {-# INLINE toWord16 #-}
  toWord32 (Bit 0) = 0
  toWord32 _       = maxBound
  {-# INLINE toWord32 #-}
  toWord64 (Bit 0) = 0
  toWord64 _       = maxBound
  {-# INLINE toWord64 #-}
  toFloat (Bit 0) = 0
  toFloat _       = 1
  {-# INLINE toFloat #-}
  toRealFloat (Bit 0) = 0
  toRealFloat _       = 1
  {-# INLINE toRealFloat #-}
  fromRealFloat 0 = zero
  fromRealFloat _ = one
  {-# INLINE fromRealFloat #-}
  (//) = cf2 div
  {-# INLINE (//) #-}


instance Num Bit where
  (+) = (.|.)
  {-# INLINE (+) #-}
  -- 0 - 0 = 0
  -- 0 - 1 = 0
  -- 1 - 0 = 1
  -- 1 - 1 = 0
  (Bit 0) - (Bit 0) = zero
  _       - (Bit 0) = one
  _       - _       = zero
  {-# INLINE (-) #-}
  (*) = (.&.)
  {-# INLINE (*) #-}
  abs         = id
  {-# INLINE abs #-}
  signum      = id
  {-# INLINE signum #-}
  fromInteger 0 = zero
  fromInteger _ = one
  {-# INLINE fromInteger #-}

-- | Unboxing of a `Bit`.
instance U.Unbox Bit

newtype instance U.MVector s Bit = MV_Bit (U.MVector s Word8)

instance M.MVector U.MVector Bit where
  basicLength (MV_Bit mvec) = M.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Bit mvec) = MV_Bit (M.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Bit mvec) (MV_Bit mvec') = M.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Bit <$> M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (Bit w) = MV_Bit <$> M.basicUnsafeReplicate len w
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Bit mvec) idx = Bit <$> M.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Bit mvec) idx (Bit w) = M.basicUnsafeWrite mvec idx w
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Bit mvec) = M.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Bit mvec) (Bit w) =  M.basicSet mvec w
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit <$> M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Bit mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit <$> V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Bit vec) = MV_Bit <$> V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Bit vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Bit vec) idx = Bit <$> V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w
  {-# INLINE elemseq #-}
