{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Color.Model.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.Internal
  ( ColorModel(..)
  , module Graphics.Color.Algebra
  , showsColorModel
  , showsColorModelOpen
  -- * Alpha
  , Alpha
  , Opaque
  , addAlpha
  , getAlpha
  , setAlpha
  , dropAlpha
  , modifyAlpha
  , modifyOpaque
  , Color(..)
  -- * Helpers
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

import qualified Control.Applicative as A
import Data.List.NonEmpty as NE
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Default.Class (Default(..))
import Data.Foldable as F
import Data.Kind
import Data.Typeable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import Graphics.Color.Algebra

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
      , Typeable (Opaque cs)
      ) =>
      ColorModel cs e where
  type Components cs e :: Type
  type ChannelCount cs :: Nat
  -- | Convert a Color to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Color cs e -> Components cs e
  -- | Convert from an elemnt representation back to a Color.
  fromComponents :: Components cs e -> Color cs e

  -- | Number of channels in the color model (eg. RGB has three). Must be a positive number.
  --
  -- @since 0.3.1
  channelCount :: Proxy (Color cs e) -> Int

  -- | Textual name for each of the channels
  --
  -- @since 0.3.1
  channelNames :: Proxy (Color cs e) -> NonEmpty String

  -- | Some 8bit sRGB values for each of the channels that might or might not have some
  -- meaningful relation to the actual colors in each channel. This is useful for plotting
  -- values.
  --
  -- @since 0.3.1
  channelColors :: Proxy (Color cs e) -> NonEmpty (V3 Word8)

  -- | Display the @cs@ portion of the pixel. Color itself will not be evaluated.
  --
  -- @since 0.1.0
  showsColorModelName :: Proxy (Color cs e) -> ShowS
  showsColorModelName _ = showsType (Proxy :: Proxy cs)


instance ColorModel cs e => Default (Color cs e) where
  def = pure 0
  {-# INLINE def #-}


instance ColorModel cs e => Num (Color cs e) where
  (+)         = A.liftA2 (+)
  {-# INLINE (+) #-}
  (-)         = A.liftA2 (-)
  {-# INLINE (-) #-}
  (*)         = A.liftA2 (*)
  {-# INLINE (*) #-}
  abs         = fmap abs
  {-# INLINE abs #-}
  signum      = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance (ColorModel cs e, Fractional e) => Fractional (Color cs e) where
  (/)          = A.liftA2 (/)
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
  basicUnsafeNew len = MV_Color <$> VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Color <$> VM.basicUnsafeReplicate len (toComponents val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Color mvec) idx = fromComponents <$> VM.basicUnsafeRead mvec idx
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
  basicUnsafeGrow (MV_Color mvec) len = MV_Color <$> VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
  basicInitialize (MV_Color mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}


newtype instance VU.Vector (Color cs e) = V_Color (VU.Vector (Components cs e))

instance (ColorModel cs e) => V.Vector VU.Vector (Color cs e) where
  basicUnsafeFreeze (MV_Color mvec) = V_Color <$> V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Color vec) = MV_Color <$> V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Color vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Color vec) = V_Color (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Color vec) idx = fromComponents <$> V.basicUnsafeIndexM vec idx
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
      case F.toList px of
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


-----------
-- Alpha --
-----------



data Alpha cs

data instance Color (Alpha cs) e = Alpha
  { _opaque :: !(Color cs e)
  , _alpha :: !e
  }

-- | Get the alpha channel value for the pixel
--
-- @since 0.1.0
getAlpha :: Color (Alpha cs) e -> e
getAlpha = _alpha
{-# INLINE getAlpha #-}

-- | Get the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
dropAlpha :: Color (Alpha cs) e -> Color cs e
dropAlpha = _opaque
{-# INLINE dropAlpha #-}

-- | Add an alpha channel value to an opaque pixel
--
-- @since 0.1.0
addAlpha :: Color cs e -> e -> Color (Alpha cs) e
addAlpha = Alpha
{-# INLINE addAlpha #-}

-- | Change the alpha channel value for the pixel
--
-- @since 0.1.0
setAlpha :: Color (Alpha cs) e -> e -> Color (Alpha cs) e
setAlpha px a = px { _alpha = a }
{-# INLINE setAlpha #-}

-- | Change the alpha channel value for the pixel
--
-- @since 0.1.0
modifyAlpha :: (e -> e) -> Color (Alpha cs) e -> Color (Alpha cs) e
modifyAlpha f px = px { _alpha = f (_alpha px) }
{-# INLINE modifyAlpha #-}

-- | Change the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
modifyOpaque :: (Color cs e -> Color cs' e) -> Color (Alpha cs) e -> Color (Alpha cs') e
modifyOpaque fpx pxa = pxa { _opaque = fpx (_opaque pxa) }
{-# INLINE modifyOpaque #-}

instance (Eq (Color cs e), Eq e) => Eq (Color (Alpha cs) e) where
  (==) (Alpha px1 a1) (Alpha px2 a2) = px1 == px2 && a1 == a2
  {-# INLINE (==) #-}

instance (ColorModel cs e, cs ~ Opaque (Alpha cs)) =>
         Show (Color (Alpha cs) e) where
  showsPrec _ = showsColorModel

type family Opaque cs where
  Opaque (Alpha (Alpha cs)) = TypeError ('Text "Nested alpha channels are not allowed")
  Opaque (Alpha cs) = cs
  Opaque cs = cs

instance (ColorModel cs e, cs ~ Opaque (Alpha cs)) =>
         ColorModel (Alpha cs) e where
  type Components (Alpha cs) e = (Components cs e, e)
  type ChannelCount (Alpha cs) = 1 + ChannelCount cs
  channelCount _ = 1 + channelCount (Proxy :: Proxy (Color cs e))
  {-# INLINE channelCount #-}
  channelNames _ = channelNames (Proxy :: Proxy (Color cs e)) <> ("Alpha" :| [])
  channelColors _ =
    channelColors (Proxy :: Proxy (Color cs e)) <> (V3 0xe6 0xe6 0xfa :| []) -- <- lavander
  toComponents (Alpha px a) = (toComponents px, a)
  {-# INLINE toComponents #-}
  fromComponents (pxc, a) = Alpha (fromComponents pxc) a
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("Alpha (" ++) . showsColorModelName (Proxy :: Proxy (Color cs e)) . (')':)


instance Functor (Color cs) => Functor (Color (Alpha cs)) where
  fmap f (Alpha px a) = Alpha (fmap f px) (f a)
  {-# INLINE fmap #-}

instance Applicative (Color cs) => Applicative (Color (Alpha cs)) where
  pure e = Alpha (pure e) e
  {-# INLINE pure #-}
  (Alpha fpx fa) <*> (Alpha px a) = Alpha (fpx <*> px) (fa a)
  {-# INLINE (<*>) #-}

instance Foldable (Color cs) => Foldable (Color (Alpha cs)) where
  foldr f acc (Alpha px a) = foldr f (f a acc) px
  {-# INLINE foldr #-}
  foldr1 f (Alpha px a) = foldr f a px
  {-# INLINE foldr1 #-}

instance Traversable (Color cs) => Traversable (Color (Alpha cs)) where
  traverse f (Alpha px a) = Alpha <$> traverse f px <*> f a
  {-# INLINE traverse #-}

instance (Storable (Color cs e), Storable e) => Storable (Color (Alpha cs) e) where
  sizeOf _ = sizeOf (undefined :: Color cs e) + sizeOf (undefined :: e)
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
