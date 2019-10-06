{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.RGB
  ( RGB
  --, RGBA
  , Pixel(..)
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal


-- | The most common @RGB@ color model
data RGB

data instance Pixel RGB e = PixelRGB !e !e !e deriving (Eq, Ord)

instance Elevator e => Show (Pixel RGB e) where
  showsPrec _ px@(PixelRGB r g b) = showsP (showsColorModel px) (shows3 r g b)

instance Elevator e => ColorModel RGB e where
  type Components RGB e = (e, e, e)
  toComponents (PixelRGB r g b) = (r, g, b)
  {-# INLINE toComponents #-}
  fromComponents (r, g, b) = PixelRGB r g b
  {-# INLINE fromComponents #-}
  showsColorModel _ = ("RGB" ++)

instance Functor (Pixel RGB) where
  fmap f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE fmap #-}

instance Applicative (Pixel RGB) where
  pure !e = PixelRGB e e e
  {-# INLINE pure #-}
  (PixelRGB fr fg fb) <*> (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel RGB) where
  foldr f !acc (PixelRGB r g b) = foldr3 f acc r g b
  {-# INLINE foldr #-}

instance Traversable (Pixel RGB) where
  traverse f (PixelRGB r g b) = traverse3 PixelRGB f r g b
  {-# INLINE traverse #-}

instance Storable e => Storable (Pixel RGB e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelRGB
  {-# INLINE peek #-}
  poke p (PixelRGB r g b) = poke3 p r g b
  {-# INLINE poke #-}

------------
--- RGBA ---
------------


-- -- | @sRGB@ color space with Alpha channel.
-- data RGBA


-- data instance Pixel RGBA e = PixelRGBA !e !e !e !e deriving (Eq, Ord)


-- instance Show e => Show (Pixel RGBA e) where
--   showsPrec _ (PixelRGBA r g b a) = showsP "sRGBA" (shows4 r g b a)


-- instance Elevator e => ColorSpace RGBA e where
--   type Components RGBA e = (e, e, e, e)

--   toComponents (PixelRGBA r g b a) = (r, g, b, a)
--   {-# INLINE toComponents #-}
--   fromComponents (r, g, b, a) = PixelRGBA r g b a
--   {-# INLINE fromComponents #-}

-- instance Elevator e => AlphaSpace RGBA e where
--   type Opaque RGBA = RGB
--   getAlpha (PixelRGBA _ _ _ a) = a
--   {-# INLINE getAlpha #-}
--   addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
--   {-# INLINE addAlpha #-}
--   dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
--   {-# INLINE dropAlpha #-}


-- instance Functor (Pixel RGBA) where
--   fmap f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
--   {-# INLINE fmap #-}

-- instance Applicative (Pixel RGBA) where
--   pure !e = PixelRGBA e e e e
--   {-# INLINE pure #-}
--   (PixelRGBA fr fg fb fa) <*> (PixelRGBA r g b a) = PixelRGBA (fr r) (fg g) (fb b) (fa a)
--   {-# INLINE (<*>) #-}

-- instance Foldable (Pixel RGBA) where
--   foldr f !z (PixelRGBA r g b a) = foldr4 f z r g b a
--   {-# INLINE foldr #-}

-- instance Traversable (Pixel RGBA) where
--   traverse f (PixelRGBA r g b a) = traverse4 PixelRGBA f r g b a
--   {-# INLINE traverse #-}

-- instance Storable e => Storable (Pixel RGBA e) where
--   sizeOf = sizeOfN 4
--   {-# INLINE sizeOf #-}
--   alignment = alignmentN 4
--   {-# INLINE alignment #-}
--   peek = peek4 PixelRGBA
--   {-# INLINE peek #-}
--   poke p (PixelRGBA r g b a) = poke4 p r g b a
--   {-# INLINE poke #-}
