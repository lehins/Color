{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  , AlphaSpace(..)
  , XYZ
  , module Graphics.ColorSpace.Elevator
  , showsP
  , shows3
  , shows4
  , shows5
  , foldr3
  , foldr4
  , foldr5
  , traverse3
  , traverse4
  , traverse5
  , sizeOfN
  , alignmentN
  , peek3
  , poke3
  , peek4
  , poke4
  , peek5
  , poke5
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
import Foreign.Ptr
import Foreign.Storable
import Graphics.ColorSpace.Elevator


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
      ColorSpace cs e
  where
  type Components cs e
  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e
  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e

  toPixelXYZ :: Pixel cs e -> Pixel XYZ Double

  fromPixelXYZ :: Pixel XYZ Double -> Pixel cs e


-- | A color space that supports transparency.
class (ColorSpace (Opaque cs) e, ColorSpace cs e) => AlphaSpace cs e where
  -- | A corresponding opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel.
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel to an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e

  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


instance ColorSpace cs e => Default (Pixel cs e) where
  def = pure 0
  {-# INLINE def #-}


instance ColorSpace cs e => Num (Pixel cs e) where
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


instance (ColorSpace cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs e, Floating e) => Floating (Pixel cs e) where
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

instance (ColorSpace cs e, Bounded e) => Bounded (Pixel cs e) where
  maxBound = pure maxBound
  {-# INLINE maxBound #-}
  minBound = pure minBound
  {-# INLINE minBound #-}

instance (ColorSpace cs e, NFData e) => NFData (Pixel cs e) where
  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}


-- | Unboxing of a `Pixel`.
instance ColorSpace cs e => VU.Unbox (Pixel cs e)

newtype instance VU.MVector s (Pixel cs e) = MV_Pixel (VU.MVector s (Components cs e))

instance ColorSpace cs e => VM.MVector VU.MVector (Pixel cs e) where
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

instance (ColorSpace cs e) => V.Vector VU.Vector (Pixel cs e) where
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


-- Show helpers

channelSeparator :: ShowS
channelSeparator = ('|':)

shows3 :: Show a => a -> a -> a -> ShowS
shows3 c0 c1 c2 = shows c0 . channelSeparator . shows c1 . channelSeparator . shows c2

shows4 :: Show a => a -> a -> a -> a -> ShowS
shows4 c0 c1 c2 c3 = shows c0 . channelSeparator . shows3 c1 c2 c3

shows5 :: Show a => a -> a -> a -> a -> a -> ShowS
shows5 c0 c1 c2 c3 c4 = shows c0 . channelSeparator . shows4 c1 c2 c3 c4

showsP :: String -> ShowS -> ShowS
showsP t i = ('<' :) . (t ++) . (":(" ++) . i . (")>" ++)

-- Foldable helpers

foldr3 :: (e -> a -> a) -> a -> e -> e -> e -> a
foldr3 f acc c0 c1 c2 = f c0 (f c1 (f c2 acc))
{-# INLINE foldr3 #-}

foldr4 :: (e -> a -> a) -> a -> e -> e -> e -> e -> a
foldr4 f acc c0 c1 c2 c3 = f c0 (f c1 (f c2 (f c3 acc)))
{-# INLINE foldr4 #-}

foldr5 :: (e -> a -> a) -> a -> e -> e -> e -> e -> e -> a
foldr5 f acc c0 c1 c2 c3 c4 = f c0 (f c1 (f c2 (f c3 (f c4 acc))))
{-# INLINE foldr5 #-}

traverse3 :: Applicative f => (a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> f b
traverse3 g f c0 c1 c2 = g <$> f c0 <*> f c1 <*> f c2
{-# INLINE traverse3 #-}

traverse4 :: Applicative f => (a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> f b
traverse4 g f c0 c1 c2 c3 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3
{-# INLINE traverse4 #-}

traverse5 ::
     Applicative f => (a -> a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> t -> f b
traverse5 g f c0 c1 c2 c3 c4 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3 <*> f c4
{-# INLINE traverse5 #-}


-- Storable helpers

sizeOfN :: forall cs e . Storable e => Int -> Pixel cs e -> Int
sizeOfN n _ = n * sizeOf (undefined :: e)
{-# INLINE sizeOfN #-}

alignmentN :: forall cs e . Storable e => Int -> Pixel cs e -> Int
alignmentN _ _ = alignment (undefined :: e)
{-# INLINE alignmentN #-}

peek3 :: Storable e => (e -> e -> e -> Pixel cs e) -> Ptr (Pixel cs e) -> IO (Pixel cs e)
peek3 f p = do
  let q = castPtr p
  c0 <- peek q
  c1 <- peekElemOff q 1
  c2 <- peekElemOff q 2
  return $! f c0 c1 c2
{-# INLINE peek3 #-}

poke3 :: Storable e => Ptr (Pixel cs e) -> e -> e -> e -> IO ()
poke3 p c0 c1 c2 = do
  let q = castPtr p
  poke q c0
  pokeElemOff q 1 c1
  pokeElemOff q 2 c2
{-# INLINE poke3 #-}

peek4 ::
     forall cs e. Storable e
  => (e -> e -> e -> e -> Pixel cs e)
  -> Ptr (Pixel cs e)
  -> IO (Pixel cs e)
peek4 f p = do
  c0 <- peek (castPtr p)
  peek3 (f c0) (p `plusPtr` sizeOf (undefined :: e))
{-# INLINE peek4 #-}

poke4 :: forall cs e . Storable e => Ptr (Pixel cs e) -> e -> e -> e -> e -> IO ()
poke4 p c0 c1 c2 c3 = do
  poke (castPtr p) c0
  poke3 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3
{-# INLINE poke4 #-}

peek5 ::
     forall cs e. Storable e
  => (e -> e -> e -> e -> e -> Pixel cs e)
  -> Ptr (Pixel cs e)
  -> IO (Pixel cs e)
peek5 f p = do
  c0 <- peek (castPtr p)
  peek4 (f c0) (p `plusPtr` sizeOf (undefined :: e))
{-# INLINE peek5 #-}

poke5 :: forall cs e . Storable e => Ptr (Pixel cs e) -> e -> e -> e -> e -> e -> IO ()
poke5 p c0 c1 c2 c3 c4 = do
  poke (castPtr p) c0
  poke4 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3 c4
{-# INLINE poke5 #-}



-----------
--- XYZ ---
-----------

-- | The original color space @CIE 1931 XYZ@ color space
data XYZ

data instance Pixel XYZ e = PixelXYZ !e !e !e deriving (Eq, Ord, Bounded)

instance Show e => Show (Pixel XYZ e) where
  showsPrec _ (PixelXYZ x y z) = showsP "XYZ" (shows3 x y z)

instance Elevator e => ColorSpace XYZ e where
  type Components XYZ e = (e, e, e)
  toComponents (PixelXYZ x y z) = (x, y, z)
  {-# INLINE toComponents #-}
  fromComponents (x, y, z) = PixelXYZ x y z
  {-# INLINE fromComponents #-}
  toPixelXYZ (PixelXYZ x y z) = PixelXYZ (eToDouble x) (eToDouble y) (eToDouble z)
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ (PixelXYZ x y z) = PixelXYZ (eFromDouble x) (eFromDouble y) (eFromDouble z)
  {-# INLINE fromPixelXYZ #-}

instance Functor (Pixel XYZ) where
  fmap f (PixelXYZ x y z) = PixelXYZ (f x) (f y) (f z)
  {-# INLINE fmap #-}

instance Applicative (Pixel XYZ) where
  pure e = PixelXYZ e e e
  {-# INLINE pure #-}
  (PixelXYZ fx fy fz) <*> (PixelXYZ x y z) = PixelXYZ (fx x) (fy y) (fz z)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel XYZ) where
  foldr f acc (PixelXYZ x y z) = foldr3 f acc x y z
  {-# INLINE foldr #-}

instance Traversable (Pixel XYZ) where
  traverse f (PixelXYZ x y z) = traverse3 PixelXYZ f x y z
  {-# INLINE traverse #-}

instance Storable e => Storable (Pixel XYZ e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelXYZ
  {-# INLINE peek #-}
  poke p (PixelXYZ x y z) = poke3 p x y z
  {-# INLINE poke #-}
