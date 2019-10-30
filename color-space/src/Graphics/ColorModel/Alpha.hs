{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.Alpha
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.Alpha
  ( Alpha
  , Opaque
  , addAlpha
  , getAlpha
  , setAlpha
  , dropAlpha
  , modifyOpaque
  , Pixel(Alpha)
  , ColorModel(..)
  ) where

import Foreign.Ptr
import Foreign.Storable
import Graphics.ColorModel.Internal
import GHC.TypeLits

data Alpha cs

data instance Pixel (Alpha cs) e = Alpha
  { _opaque :: !(Pixel cs e)
  , _alpha :: !e
  }

-- | Get the alpha channel value for the pixel
--
-- @since 0.1.0
getAlpha :: Pixel (Alpha cs) e -> e
getAlpha = _alpha
{-# INLINE getAlpha #-}

-- | Get the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
dropAlpha :: Pixel (Alpha cs) e -> Pixel cs e
dropAlpha = _opaque
{-# INLINE dropAlpha #-}

-- | Add an alpha channel value to an opaque pixel
--
-- @since 0.1.0
addAlpha :: Pixel cs e -> e -> Pixel (Alpha cs) e
addAlpha = Alpha
{-# INLINE addAlpha #-}

-- | Change the alpha channel value for the pixel
--
-- @since 0.1.0
setAlpha :: Pixel (Alpha cs) e -> e -> Pixel (Alpha cs) e
setAlpha px a = px { _alpha = a }
{-# INLINE setAlpha #-}

-- | Change the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
modifyOpaque :: (Pixel cs e -> Pixel cs' e) -> Pixel (Alpha cs) e -> Pixel (Alpha cs') e
modifyOpaque fpx pxa = pxa { _opaque = fpx (_opaque pxa) }
{-# INLINE modifyOpaque #-}

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
