{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Model.CMYK
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.CMYK
  ( CMYK
    -- * Constructors for an CMYK color model.
  , pattern PixelCMYK
  , pattern PixelCMYKA
  , Pixel
  , ColorModel(..)
  , cmyk2rgb
  , rgb2cmyk
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB

------------
--- CMYK ---
------------

data CMYK

-- | `CMYK` color model
data instance Pixel CMYK e = PixelCMYK !e !e !e !e

-- | Constructor for @CMYK@ with alpha channel.
pattern PixelCMYKA :: e -> e -> e -> e -> e -> Pixel (Alpha CMYK) e
pattern PixelCMYKA c m y k a = Alpha (PixelCMYK c m y k) a
{-# COMPLETE PixelCMYKA #-}

-- | `CMYK` color model
deriving instance Eq e => Eq (Pixel CMYK e)
-- | `CMYK` color model
deriving instance Ord e => Ord (Pixel CMYK e)

-- | `CMYK` color model
instance Elevator e => Show (Pixel CMYK e) where
  showsPrec _ = showsColorModel

-- | `CMYK` color model
instance Elevator e => ColorModel CMYK e where
  type Components CMYK e = (e, e, e, e)
  toComponents (PixelCMYK c m y k) = (c, m, y, k)
  {-# INLINE toComponents #-}
  fromComponents (c, m, y, k) = PixelCMYK c m y k
  {-# INLINE fromComponents #-}

-- | `CMYK` color model
instance Functor (Pixel CMYK) where
  fmap f (PixelCMYK c m y k) = PixelCMYK (f c) (f m) (f y) (f k)
  {-# INLINE fmap #-}

-- | `CMYK` color model
instance Applicative (Pixel CMYK) where
  pure !e = PixelCMYK e e e e
  {-# INLINE pure #-}
  (PixelCMYK fc fm fy fk) <*> (PixelCMYK c m y k) = PixelCMYK (fc c) (fm m) (fy y) (fk k)
  {-# INLINE (<*>) #-}

-- | `CMYK` color model
instance Foldable (Pixel CMYK) where
  foldr f !z (PixelCMYK c m y k) = f c (f m (f y (f k z)))
  {-# INLINE foldr #-}

-- | `CMYK` color model
instance Traversable (Pixel CMYK) where
  traverse f (PixelCMYK c m y k) = PixelCMYK <$> f c <*> f m <*> f y <*> f k
  {-# INLINE traverse #-}

-- | `CMYK` color model
instance Storable e => Storable (Pixel CMYK e) where
  sizeOf = sizeOfN 4
  {-# INLINE sizeOf #-}
  alignment = alignmentN 4
  {-# INLINE alignment #-}
  peek = peek4 PixelCMYK
  {-# INLINE peek #-}
  poke p (PixelCMYK c m y k) = poke4 p c m y k
  {-# INLINE poke #-}

cmyk2rgb :: Num e => Pixel CMYK e -> Pixel RGB e
cmyk2rgb (PixelCMYK c m y k) = PixelRGB r g b
  where
    !r = (1 - c) * (1 - k)
    !g = (1 - m) * (1 - k)
    !b = (1 - y) * (1 - k)
{-# INLINE cmyk2rgb #-}


rgb2cmyk :: (Ord e, Fractional e) => Pixel RGB e -> Pixel CMYK e
rgb2cmyk (PixelRGB r g b) = PixelCMYK c m y k
  where
    !c = (k' - r) / k'
    !m = (k' - g) / k'
    !y = (k' - b) / k'
    !k = 1 - k'
    !k' = max r (max g b)
{-# INLINE rgb2cmyk #-}
