{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.CIE1931.XYZ
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.CIE1931.XYZ (
  XYZ, Pixel(..)
  ) where

import Graphics.ColorSpace.Internal



-- ------------
-- --- XYZA ---
-- ------------


-- -- | @sXYZ@ color space with Alpha channel.
-- data XYZA


-- data instance Pixel XYZA e = PixelXYZA !e !e !e !e deriving (Eq, Ord)


-- instance Show e => Show (Pixel XYZA e) where
--   showsPrec _ (PixelXYZA x y z a) = showsP "sXYZA" (shows4 x y z a)


-- instance Elevator e => ColorSpace XYZA e where
--   type Components XYZA e = (e, e, e, e)

--   toComponents (PixelXYZA x y z a) = (r, x, y, z)
--   {-# INLINE toComponents #-}
--   fromComponents (r, g, b, a) = PixelXYZA x y z a
--   {-# INLINE fromComponents #-}

-- instance Elevator e => AlphaSpace XYZA e where
--   type Opaque XYZA = XYZ
--   getAlpha (PixelXYZA _ _ _ a) = a
--   {-# INLINE getAlpha #-}
--   addAlpha !a (PixelXYZ x y z) = PixelXYZA x y z a
--   {-# INLINE addAlpha #-}
--   dropAlpha (PixelXYZA x y z _) = PixelXYZ x y z
--   {-# INLINE dropAlpha #-}


-- instance Functor (Pixel XYZA) where
--   fmap f (PixelXYZA x y z a) = PixelXYZA (f r) (f g) (f b) (f a)
--   {-# INLINE fmap #-}

-- instance Applicative (Pixel XYZA) where
--   pure !e = PixelXYZA e e e e
--   {-# INLINE pure #-}
--   (PixelXYZA fr fg fb fa) <*> (PixelXYZA x y z a) = PixelXYZA (fr r) (fg g) (fb b) (fa a)
--   {-# INLINE (<*>) #-}

-- instance Foldable (Pixel XYZA) where
--   foldr f !z (PixelXYZA x y z a) = foldr4 f z x y z a
--   {-# INLINE foldr #-}

-- instance Traversable (Pixel XYZA) where
--   traverse f (PixelXYZA x y z a) = traverse4 PixelXYZA f x y z a
--   {-# INLINE traverse #-}

-- instance Storable e => Storable (Pixel XYZA e) where
--   sizeOf = sizeOfN 4
--   {-# INLINE sizeOf #-}
--   alignment = alignmentN 4
--   {-# INLINE alignment #-}
--   peek = peek4 PixelXYZA
--   {-# INLINE peek #-}
--   poke p (PixelXYZA x y z a) = poke4 p x y z a
--   {-# INLINE poke #-}
