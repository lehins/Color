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
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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

import Control.Monad
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.Storable
import Graphics.Color.Algebra.Elevator
import Prelude hiding (map)


-- | Under the hood, binary pixels are backed by `Word8`, but can only take
-- values of @0@ or @1@. Use `zero`\/`one` to construct a bit and `on`\/`off` to
-- construct a binary pixel.
newtype Bit = Bit Word8 deriving (Ord, Eq, Storable)


instance Show Bit where
  show (Bit 0) = "0"
  show _       = "1"


instance Bits Bit where
  (Bit 0) .&. _       = Bit 0
  (Bit 1) .&. (Bit 1) = Bit 1
  _       .&. (Bit 0) = Bit 0
  _       .&. _       = Bit 1
  {-# INLINE (.&.) #-}
  (Bit 1) .|. _       = Bit 1
  (Bit 0) .|. (Bit 0) = Bit 0
  _       .|. _       = Bit 1
  {-# INLINE (.|.) #-}
  (Bit 0) `xor` (Bit 0) = Bit 0
  (Bit 1) `xor` (Bit 1) = Bit 0
  _       `xor` _       = Bit 1
  {-# INLINE xor #-}
  complement (Bit 0) = Bit 1
  complement       _ = Bit 0
  {-# INLINE complement #-}
  shift !b 0 = b
  shift  _ _ = Bit 0
  {-# INLINE shift #-}
  rotate !b _ = b
  {-# INLINE rotate #-}
  zeroBits = Bit 0
  {-# INLINE zeroBits #-}
  bit 0 = Bit 1
  bit _ = Bit 0
  {-# INLINE bit #-}
  testBit (Bit 1) 0 = True
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
zero = Bit 0
{-# INLINE zero #-}

one :: Bit
one = Bit 1
{-# INLINE one #-}


-- | Values: @0@ and @1@
instance Elevator Bit where
  minValue = Bit 0
  {-# INLINE minValue #-}
  maxValue = Bit 1
  {-# INLINE maxValue #-}
  toShowS (Bit 0) = ('0':)
  toShowS _       = ('1':)
  toWord8 (Bit 0) = 0
  toWord8 _       = maxBound
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
  fromRealFloat 0 = Bit 0
  fromRealFloat _ = Bit 1
  {-# INLINE fromRealFloat #-}


instance Num Bit where
  (+) = (.|.)
  {-# INLINE (+) #-}
  -- 0 - 0 = 0
  -- 0 - 1 = 0
  -- 1 - 0 = 1
  -- 1 - 1 = 0
  (Bit 0) - (Bit 0) = Bit 0
  _       - (Bit 0) = Bit 1
  _       - _       = Bit 0
  {-# INLINE (-) #-}
  (*) = (.&.)
  {-# INLINE (*) #-}
  abs         = id
  {-# INLINE abs #-}
  signum      = id
  {-# INLINE signum #-}
  fromInteger 0 = Bit 0
  fromInteger _ = Bit 1
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
  basicUnsafeNew len = MV_Bit `liftM` M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (Bit w) = MV_Bit `liftM` M.basicUnsafeReplicate len w
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Bit mvec) idx = Bit `liftM` M.basicUnsafeRead mvec idx
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
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit `liftM` M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Bit mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Bit vec) = MV_Bit `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Bit vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Bit vec) idx = Bit `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w
  {-# INLINE elemseq #-}
