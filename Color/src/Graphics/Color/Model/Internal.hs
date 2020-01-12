{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Model.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.Internal
  ( Color
  , ColorModel(..)
  , module Graphics.Color.Algebra
  , showsColorModel
  , showsColorModelOpen
  , foldr3
  , foldr4
  , traverse3
  , traverse4
  , sizeOfN
  , alignmentN
  , peek3
  , poke3
  , peek4
  , poke4
  , VU.MVector(MV_Color)
  , VU.Vector(V_Color)
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
import Graphics.Color.Algebra
import Data.Kind

-- | A Color family with a color space and a precision of elements.
data family Color cs e :: Type

class ( Functor (Color cs)
      , Applicative (Color cs)
      , Foldable (Color cs)
      , Traversable (Color cs)
      , Eq (Color cs e)
      , Show (Color cs e)
      , VU.Unbox (Components cs e)
      , VS.Storable (Color cs e)
      , Typeable cs
      , Elevator e
      ) =>
      ColorModel cs e where
  type Components cs e :: Type
  -- | Convert a Color to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Color cs e -> Components cs e
  -- | Convert from an elemnt representation back to a Color.
  fromComponents :: Components cs e -> Color cs e

  -- | Display the @cs@ portion of the pixel. Color itself will not be evaluated.
  --
  -- @since 0.1.0
  showsColorModelName :: Proxy (Color cs e) -> ShowS
  showsColorModelName _ = showsType (Proxy :: Proxy cs)


instance ColorModel cs e => Default (Color cs e) where
  def = pure 0
  {-# INLINE def #-}


instance ColorModel cs e => Num (Color cs e) where
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


instance (ColorModel cs e, Fractional e) => Fractional (Color cs e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance (ColorModel cs e, Floating e) => Floating (Color cs e) where
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

instance ColorModel cs e => Bounded (Color cs e) where
  maxBound = pure maxValue
  {-# INLINE maxBound #-}
  minBound = pure minValue
  {-# INLINE minBound #-}

instance (ColorModel cs e, NFData e) => NFData (Color cs e) where
  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}


-- | Unboxing of a `Color`.
instance ColorModel cs e => VU.Unbox (Color cs e)

newtype instance VU.MVector s (Color cs e) = MV_Color (VU.MVector s (Components cs e))

instance ColorModel cs e => VM.MVector VU.MVector (Color cs e) where
  basicLength (MV_Color mvec) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Color mvec) = MV_Color (VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Color mvec) (MV_Color mvec') = VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Color `liftM` VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Color `liftM` VM.basicUnsafeReplicate len (toComponents val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Color mvec) idx = fromComponents `liftM` VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Color mvec) idx val = VM.basicUnsafeWrite mvec idx (toComponents val)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Color mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Color mvec) val = VM.basicSet mvec (toComponents val)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Color mvec) (MV_Color mvec') = VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Color mvec) (MV_Color mvec') = VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Color mvec) len = MV_Color `liftM` VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Color mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector (Color cs e) = V_Color (VU.Vector (Components cs e))

instance (ColorModel cs e) => V.Vector VU.Vector (Color cs e) where
  basicUnsafeFreeze (MV_Color mvec) = V_Color `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Color vec) = MV_Color `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Color vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Color vec) = V_Color (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Color vec) idx = fromComponents `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Color mvec) (V_Color vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Color vec) val = V.elemseq vec (toComponents val)
  {-# INLINE elemseq #-}

channelSeparator :: Char
channelSeparator = ','

showsColorModel :: ColorModel cs e => Color cs e -> ShowS
showsColorModel px = ('<' :) . showsColorModelOpen px . ('>' :)

showsColorModelOpen :: ColorModel cs e => Color cs e -> ShowS
showsColorModelOpen px = t . (":(" ++) . channels . (')' :)
  where
    t = asProxy px showsColorModelName
    channels =
      case toList px of
        [] -> id
        (x:xs) -> foldl' (\facc y -> facc . (channelSeparator :) . toShowS y) (toShowS x) xs

-- TODO: consolidate those helpers into algebra by means of: V2, V3, V4 and V5.
-- Foldable helpers.

foldr3 :: (e -> a -> a) -> a -> e -> e -> e -> a
foldr3 f acc c0 c1 c2 = f c0 (f c1 (f c2 acc))
{-# INLINE foldr3 #-}

foldr4 :: (e -> a -> a) -> a -> e -> e -> e -> e -> a
foldr4 f acc c0 c1 c2 c3 = f c0 (f c1 (f c2 (f c3 acc)))
{-# INLINE foldr4 #-}

traverse3 :: Applicative f => (a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> f b
traverse3 g f c0 c1 c2 = g <$> f c0 <*> f c1 <*> f c2
{-# INLINE traverse3 #-}

traverse4 :: Applicative f => (a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> f b
traverse4 g f c0 c1 c2 c3 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3
{-# INLINE traverse4 #-}

-- Storable helpers

sizeOfN :: forall cs e . Storable e => Int -> Color cs e -> Int
sizeOfN n _ = n * sizeOf (undefined :: e)
{-# INLINE sizeOfN #-}

alignmentN :: forall cs e . Storable e => Int -> Color cs e -> Int
alignmentN _ _ = alignment (undefined :: e)
{-# INLINE alignmentN #-}

peek3 :: Storable e => (e -> e -> e -> Color cs e) -> Ptr (Color cs e) -> IO (Color cs e)
peek3 f p = do
  let q = castPtr p
  c0 <- peek q
  c1 <- peekElemOff q 1
  c2 <- peekElemOff q 2
  return $! f c0 c1 c2
{-# INLINE peek3 #-}

poke3 :: Storable e => Ptr (Color cs e) -> e -> e -> e -> IO ()
poke3 p c0 c1 c2 = do
  let q = castPtr p
  poke q c0
  pokeElemOff q 1 c1
  pokeElemOff q 2 c2
{-# INLINE poke3 #-}

peek4 ::
     forall cs e. Storable e
  => (e -> e -> e -> e -> Color cs e)
  -> Ptr (Color cs e)
  -> IO (Color cs e)
peek4 f p = do
  c0 <- peek (castPtr p)
  peek3 (f c0) (p `plusPtr` sizeOf (undefined :: e))
{-# INLINE peek4 #-}

poke4 :: forall cs e . Storable e => Ptr (Color cs e) -> e -> e -> e -> e -> IO ()
poke4 p c0 c1 c2 c3 = do
  poke (castPtr p) c0
  poke3 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3
{-# INLINE poke4 #-}
