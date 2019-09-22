{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.Internal
  ( Pixel
  , ColorModel(..)
  -- , AlphaSpace(..)
  , module Graphics.ColorModel.Elevator
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf), deepseq)
import Control.Monad (liftM)
import Data.Default.Class (Default(..))
import Data.Foldable
import Data.Typeable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Graphics.ColorModel.Elevator


-- | A Pixel family with a color space and a precision of elements.
data family Pixel cs e :: *

class ( Functor (Pixel cs)
      , Applicative (Pixel cs)
      , Foldable (Pixel cs)
      , Traversable (Pixel cs)
      , Eq (Pixel cs e)
      , VU.Unbox (Components cs e)
      , VS.Storable (Pixel cs e)
      , Typeable cs
      , Elevator e
      ) =>
      ColorModel cs e where
  type Components cs e
  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e
  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e


-- -- | A color space that supports transparency.
-- class (ColorModel (Opaque cs) e, ColorModel cs e) => AlphaModel cs e where
--   -- | A corresponding opaque version of this color space.
--   type Opaque cs

--   -- | Get an alpha channel of a transparant pixel.
--   getAlpha :: Pixel cs e -> e

--   -- | Add an alpha channel to an opaque pixel.
--   --
--   -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
--   addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e

--   -- | Convert a transparent pixel to an opaque one by dropping the alpha
--   -- channel.
--   --
--   -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
--   --
--   dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


instance ColorModel cs e => Default (Pixel cs e) where
  def = pure 0
  {-# INLINE def #-}


instance ColorModel cs e => Num (Pixel cs e) where
  (+)         = liftA2 (+)
  {-# INLINE (+) #-}
  (-)         = liftA2 (-)
  {-# INLINE (-) #-}
  (*)         = liftA2 (*)
  {-# INLINE (*) #-}
  abs         = fmap abs
  {-# INLINE abs #-}
  signum      = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance (ColorModel cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance (ColorModel cs e, Floating e) => Floating (Pixel cs e) where
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

instance (ColorModel cs e, Bounded e) => Bounded (Pixel cs e) where
  maxBound = pure maxBound
  {-# INLINE maxBound #-}
  minBound = pure minBound
  {-# INLINE minBound #-}

instance (ColorModel cs e, NFData e) => NFData (Pixel cs e) where
  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}


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
  basicUnsafeNew len = MV_Pixel `liftM` VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Pixel `liftM` VM.basicUnsafeReplicate len (toComponents val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Pixel mvec) idx = fromComponents `liftM` VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Pixel mvec) idx val = VM.basicUnsafeWrite mvec idx (toComponents val)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Pixel mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Pixel mvec) val = VM.basicSet mvec (toComponents val)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Pixel mvec) len = MV_Pixel `liftM` VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Pixel mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector (Pixel cs e) = V_Pixel (VU.Vector (Components cs e))

instance (ColorModel cs e) => V.Vector VU.Vector (Pixel cs e) where
  basicUnsafeFreeze (MV_Pixel mvec) = V_Pixel `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Pixel vec) = MV_Pixel `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Pixel vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Pixel vec) = V_Pixel (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Pixel vec) idx = fromComponents `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Pixel mvec) (V_Pixel vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Pixel vec) val = V.elemseq vec (toComponents val)
  {-# INLINE elemseq #-}
