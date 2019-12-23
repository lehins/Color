{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Model.Alpha
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.Alpha
  ( Alpha
  , Opaque
  , addAlpha
  , getAlpha
  , setAlpha
  , dropAlpha
  , modifyOpaque
  , Color(Alpha)
  , ColorModel(..)
  ) where

import Foreign.Ptr
import Foreign.Storable
import Graphics.Color.Model.Internal
import GHC.TypeLits

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

-- | Change the opaque pixel value, while leaving alpha channel intact.
--
-- @since 0.1.0
modifyOpaque :: (Color cs e -> Color cs' e) -> Color (Alpha cs) e -> Color (Alpha cs') e
modifyOpaque fpx pxa = pxa { _opaque = fpx (_opaque pxa) }
{-# INLINE modifyOpaque #-}

instance (Eq (Color cs e), Eq e) => Eq (Color (Alpha cs) e) where
  (==) (Alpha px1 a1) (Alpha px2 a2) = px1 == px2 && a1 == a2
  {-# INLINE (==) #-}

instance (ColorModel cs e, Opaque (Alpha cs) ~ cs) => Show (Color (Alpha cs) e) where
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
  showsColorModelName _ = showsColorModelName (pure 0 :: Color cs e) . ('A':)


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
