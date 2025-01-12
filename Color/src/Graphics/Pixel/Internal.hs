{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
-- |
-- Module      : Graphics.Pixel.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Pixel.Internal
  ( Pixel(..)
  , liftPixel
  , toPixel8
  , toPixel16
  , toPixel32
  , toPixel64
  , toPixelF
  , toPixelD
  , VU.MVector(MV_Pixel)
  , VU.Vector(V_Pixel)
  , module Graphics.Color.Model.Internal
  ) where

import Data.Coerce
import Control.DeepSeq (NFData)
import Graphics.Color.Model.Internal
import Foreign.Storable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import Data.Default.Class (Default)

-- | Digital imaging is one of the most common places for a color to be used in. The
-- smallest element in any image is a pixel, which is defined by its color.
--
-- @since 0.1.0
newtype Pixel cs e = Pixel
  { pixelColor :: Color cs e
  -- ^ Get to the underlying `Color`
  --
  -- @since 0.1.4
  }

deriving instance Eq (Color cs e) => Eq (Pixel cs e)
deriving instance Ord (Color cs e) => Ord (Pixel cs e)
deriving instance Num (Color cs e) => Num (Pixel cs e)
deriving instance Bounded (Color cs e) => Bounded (Pixel cs e)
deriving instance NFData (Color cs e) => NFData (Pixel cs e)
deriving instance Floating (Color cs e) => Floating (Pixel cs e)
deriving instance Fractional (Color cs e) => Fractional (Pixel cs e)
deriving instance Functor (Color cs) => Functor (Pixel cs)
deriving instance Applicative (Color cs) => Applicative (Pixel cs)
deriving instance Foldable (Color cs) => Foldable (Pixel cs)
deriving instance Traversable (Color cs) => Traversable (Pixel cs)
deriving instance Storable (Color cs e) => Storable (Pixel cs e)
deriving instance Default (Color cs e) => Default (Pixel cs e)
instance Show (Color cs e) => Show (Pixel cs e) where
  show = show . pixelColor

-- | Unboxing of a `Pixel`.
instance ColorModel cs e => VU.Unbox (Pixel cs e)

newtype instance VU.MVector s (Pixel cs e) = MV_Pixel (VU.MVector s (Components cs e))

instance ColorModel cs e => VM.MVector VU.MVector (Pixel cs e) where
  basicLength (MV_Pixel mvec) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Pixel mvec) = MV_Pixel (VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Pixel <$> VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val =
    MV_Pixel <$> VM.basicUnsafeReplicate len (toComponents (coerce val))
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Pixel mvec) idx = coerce . fromComponents <$> VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Pixel mvec) idx val = VM.basicUnsafeWrite mvec idx (toComponents (coerce val))
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Pixel mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Pixel mvec) val = VM.basicSet mvec (toComponents (coerce val))
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Pixel mvec) len = MV_Pixel <$> VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
  basicInitialize (MV_Pixel mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}


newtype instance VU.Vector (Pixel cs e) = V_Pixel (VU.Vector (Components cs e))

instance (ColorModel cs e) => V.Vector VU.Vector (Pixel cs e) where
  basicUnsafeFreeze (MV_Pixel mvec) = V_Pixel <$> V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Pixel vec) = MV_Pixel <$> V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Pixel vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Pixel vec) = V_Pixel (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Pixel vec) idx = coerce . fromComponents <$> V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Pixel mvec) (V_Pixel vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Pixel vec) val = V.elemseq vec (toComponents (coerce val))
  {-# INLINE elemseq #-}

-- | Apply a function to `Pixel`'s `Color`
--
-- @since 0.1.0
liftPixel :: (Color cs e -> Color cs' e') -> Pixel cs e -> Pixel cs' e'
liftPixel f = coerce . f . coerce
{-# INLINE liftPixel #-}



-- Elevation

-- | Convert all channels of a pixel to 8bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel8 :: ColorModel cs e => Pixel cs e -> Pixel cs Word8
toPixel8 = liftPixel (fmap toWord8)
{-# INLINE toPixel8 #-}

-- | Convert all channels of a pixel to 16bits each, while appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel16 :: ColorModel cs e => Pixel cs e -> Pixel cs Word16
toPixel16 = liftPixel (fmap toWord16)
{-# INLINE toPixel16 #-}


-- | Convert all channels of a pixel to 32bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel32 :: ColorModel cs e => Pixel cs e -> Pixel cs Word32
toPixel32 = liftPixel (fmap toWord32)
{-# INLINE toPixel32 #-}


-- | Convert all channels of a pixel to 64bits each, while doing appropriate scaling. See
-- `Elevator`.
--
-- @since 0.1.0
toPixel64 :: ColorModel cs e => Pixel cs e -> Pixel cs Word64
toPixel64 = liftPixel (fmap toWord64)
{-# INLINE toPixel64 #-}


-- | Convert all channels of a pixel to 32bit floating point numers each, while doing
-- appropriate scaling. See `Elevator`.
--
-- @since 0.1.0
toPixelF :: ColorModel cs e => Pixel cs e -> Pixel cs Float
toPixelF = liftPixel (fmap toFloat)
{-# INLINE toPixelF #-}

-- | Convert all channels of a pixel to 64bit floating point numers each, while doing
-- appropriate scaling. See `Elevator`.
--
-- @since 0.1.0
toPixelD :: ColorModel cs e => Pixel cs e -> Pixel cs Double
toPixelD = liftPixel (fmap toDouble)
{-# INLINE toPixelD #-}
