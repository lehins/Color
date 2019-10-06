{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( Pixel(Alpha)
  , ColorModel(..)
  , Alpha
  , alpha
  , opaque
  , setAlpha
  , setOpaque
  , module Graphics.ColorModel.Elevator
  -- , showsP
  -- , shows3
  -- , shows4
  -- , shows5
  , showsColorModel
  , showsColorModelOpen
  , showsType
  , foldr3
  , foldr4
  -- , foldr5
  , traverse3
  , traverse4
  -- , traverse5
  , sizeOfN
  , alignmentN
  , peek3
  , poke3
  , peek4
  , poke4
  -- , peek5
  -- , poke5
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf), deepseq)
import Control.Monad (liftM)
import Data.Default.Class (Default(..))
import Data.Foldable
import Data.List as L
import Data.Typeable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Foreign.Ptr
import Foreign.Storable
import Graphics.ColorModel.Elevator
import GHC.TypeLits


-- | A Pixel family with a color space and a precision of elements.
data family Pixel cs e :: *

class ( Functor (Pixel cs)
      , Applicative (Pixel cs)
      , Foldable (Pixel cs)
      , Traversable (Pixel cs)
      , Eq (Pixel cs e)
      , VU.Unbox (Components cs e)
      , VS.Storable (Pixel cs e)
      , Elevator e
      ) =>
      ColorModel cs e where
  type Components cs e
  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e
  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e

  -- | Display the @cs@ portion of the pixel. Pixel itself will not be evaluated.
  --
  -- @since 0.1.0
  showsColorModelName :: Pixel cs e -> ShowS
  default showsColorModelName :: Typeable cs => Pixel cs e -> ShowS
  showsColorModelName _ = showsType (Proxy :: Proxy cs)

data Alpha cs

data instance Pixel (Alpha cs) e = Alpha
  { opaque :: !(Pixel cs e)
  , alpha :: !e
  }

-- | Change the alpha channel value for the pixel
--
-- @since 0.1.0
setAlpha :: Pixel (Alpha cs) e -> e -> Pixel (Alpha cs) e
setAlpha px a = px { alpha = a }

-- | Change the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
setOpaque :: Pixel (Alpha cs) e -> Pixel cs e -> Pixel (Alpha cs) e
setOpaque pxa px = pxa { opaque = px }

instance (Eq (Pixel cs e), Eq e) => Eq (Pixel (Alpha cs) e) where
  (==) (Alpha px1 a1) (Alpha px2 a2) = px1 == px2 && a1 == a2
  {-# INLINE (==) #-}

instance (ColorModel cs e, Opaque (Alpha cs) ~ cs) => Show (Pixel (Alpha cs) e) where
  showsPrec _ = showsColorModel

type family Opaque cs where
  Opaque (Alpha (Alpha cs)) = TypeError ('Text "Nested alpha channels are not allowed")
  Opaque (Alpha cs) = cs

instance (ColorModel cs e, Opaque (Alpha cs) ~ cs) => ColorModel (Alpha cs) e where
  type Components (Alpha cs) e = (Components cs e, e)
  toComponents (Alpha px a) = (toComponents px, a)
  {-# INLINE toComponents #-}
  fromComponents (pxc, a) = Alpha (fromComponents pxc) a
  {-# INLINE fromComponents #-}
  showsColorModelName _ = showsColorModelName (pure 0 :: Pixel cs e) . ('A':)


instance Functor (Pixel cs) => Functor (Pixel (Alpha cs)) where
  fmap f (Alpha px a) = Alpha (fmap f px) (f a)
  {-# INLINE fmap #-}

instance Applicative (Pixel cs) => Applicative (Pixel (Alpha cs)) where
  pure e = Alpha (pure e) e
  {-# INLINE pure #-}
  (Alpha fpx fa) <*> (Alpha px a) = Alpha (fpx <*> px) (fa a)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel cs) => Foldable (Pixel (Alpha cs)) where
  foldr f acc (Alpha px a) = foldr f (f a acc) px
  {-# INLINE foldr #-}
  foldr1 f (Alpha px a) = foldr f a px
  {-# INLINE foldr1 #-}

instance Traversable (Pixel cs) => Traversable (Pixel (Alpha cs)) where
  traverse f (Alpha px a) = Alpha <$> traverse f px <*> f a
  {-# INLINE traverse #-}

instance (Storable (Pixel cs e), Storable e) => Storable (Pixel (Alpha cs) e) where
  sizeOf _ = sizeOf (undefined :: Pixel cs e) + sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek ptr = do
    px <- peek (castPtr ptr)
    Alpha px <$> peekByteOff ptr (sizeOf px)
  {-# INLINE peek #-}
  poke ptr (Alpha px a) = do
    poke (castPtr ptr) px
    pokeByteOff ptr (sizeOf px) a
  {-# INLINE poke #-}

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

channelSeparator :: Char
channelSeparator = '|'

-- channelSeparatorS :: ShowS
-- channelSeparatorS = (channelSeparator:)

-- shows3 :: Show a => a -> a -> a -> ShowS
-- shows3 c0 c1 c2 = shows c0 . channelSeparatorS . shows c1 . channelSeparatorS . shows c2

-- shows4 :: Show a => a -> a -> a -> a -> ShowS
-- shows4 c0 c1 c2 c3 = shows c0 . channelSeparatorS . shows3 c1 c2 c3

-- shows5 :: Show a => a -> a -> a -> a -> a -> ShowS
-- shows5 c0 c1 c2 c3 c4 = shows c0 . channelSeparatorS . shows4 c1 c2 c3 c4

-- showsP :: ShowS -> ShowS -> ShowS
-- showsP t i = ('<' :) . t . (":(" ++) . i . (")>" ++)

-- showsColorSpace :: (ColorSpace cs e, Show e) => Pixel cs e -> ShowS
-- showsColorSpace px = ('<' :) . showsColorSpaceName px . (' ' :) . showsColorModelOpen px . ('>' :)


showsColorModel :: ColorModel cs e => Pixel cs e -> ShowS
showsColorModel px = ('<' :) . showsColorModelOpen px . ('>' :)

showsColorModelOpen :: ColorModel cs e => Pixel cs e -> ShowS
showsColorModelOpen px = t . (":(" ++) . (channels ++) . (')' :)
  where
    t = showsColorModelName px
    channels = L.intercalate [channelSeparator] $ map show $ toList px

showsType :: Typeable t => Proxy t -> ShowS
showsType = showsTypeRep . typeRep

-- Foldable helpers

foldr3 :: (e -> a -> a) -> a -> e -> e -> e -> a
foldr3 f acc c0 c1 c2 = f c0 (f c1 (f c2 acc))
{-# INLINE foldr3 #-}

foldr4 :: (e -> a -> a) -> a -> e -> e -> e -> e -> a
foldr4 f acc c0 c1 c2 c3 = f c0 (f c1 (f c2 (f c3 acc)))
{-# INLINE foldr4 #-}

-- foldr5 :: (e -> a -> a) -> a -> e -> e -> e -> e -> e -> a
-- foldr5 f acc c0 c1 c2 c3 c4 = f c0 (f c1 (f c2 (f c3 (f c4 acc))))
-- {-# INLINE foldr5 #-}

traverse3 :: Applicative f => (a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> f b
traverse3 g f c0 c1 c2 = g <$> f c0 <*> f c1 <*> f c2
{-# INLINE traverse3 #-}

traverse4 :: Applicative f => (a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> f b
traverse4 g f c0 c1 c2 c3 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3
{-# INLINE traverse4 #-}

-- traverse5 ::
--      Applicative f => (a -> a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> t -> f b
-- traverse5 g f c0 c1 c2 c3 c4 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3 <*> f c4
-- {-# INLINE traverse5 #-}


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

-- peek5 ::
--      forall cs e. Storable e
--   => (e -> e -> e -> e -> e -> Pixel cs e)
--   -> Ptr (Pixel cs e)
--   -> IO (Pixel cs e)
-- peek5 f p = do
--   c0 <- peek (castPtr p)
--   peek4 (f c0) (p `plusPtr` sizeOf (undefined :: e))
-- {-# INLINE peek5 #-}

-- poke5 :: forall cs e . Storable e => Ptr (Pixel cs e) -> e -> e -> e -> e -> e -> IO ()
-- poke5 p c0 c1 c2 c3 c4 = do
--   poke (castPtr p) c0
--   poke4 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3 c4
-- {-# INLINE poke5 #-}
