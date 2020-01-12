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
  , pattern ColorCMYK
  , pattern ColorCMYKA
  , Color
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
data instance Color CMYK e = ColorCMYK !e !e !e !e

-- | Constructor for @CMYK@ with alpha channel.
pattern ColorCMYKA :: e -> e -> e -> e -> e -> Color (Alpha CMYK) e
pattern ColorCMYKA c m y k a = Alpha (ColorCMYK c m y k) a
{-# COMPLETE ColorCMYKA #-}

-- | `CMYK` color model
deriving instance Eq e => Eq (Color CMYK e)
-- | `CMYK` color model
deriving instance Ord e => Ord (Color CMYK e)

-- | `CMYK` color model
instance Elevator e => Show (Color CMYK e) where
  showsPrec _ = showsColorModel

-- | `CMYK` color model
instance Elevator e => ColorModel CMYK e where
  type Components CMYK e = (e, e, e, e)
  toComponents (ColorCMYK c m y k) = (c, m, y, k)
  {-# INLINE toComponents #-}
  fromComponents (c, m, y, k) = ColorCMYK c m y k
  {-# INLINE fromComponents #-}

-- | `CMYK` color model
instance Functor (Color CMYK) where
  fmap f (ColorCMYK c m y k) = ColorCMYK (f c) (f m) (f y) (f k)
  {-# INLINE fmap #-}

-- | `CMYK` color model
instance Applicative (Color CMYK) where
  pure !e = ColorCMYK e e e e
  {-# INLINE pure #-}
  (ColorCMYK fc fm fy fk) <*> (ColorCMYK c m y k) = ColorCMYK (fc c) (fm m) (fy y) (fk k)
  {-# INLINE (<*>) #-}

-- | `CMYK` color model
instance Foldable (Color CMYK) where
  foldr f !z (ColorCMYK c m y k) = f c (f m (f y (f k z)))
  {-# INLINE foldr #-}

-- | `CMYK` color model
instance Traversable (Color CMYK) where
  traverse f (ColorCMYK c m y k) = ColorCMYK <$> f c <*> f m <*> f y <*> f k
  {-# INLINE traverse #-}

-- | `CMYK` color model
instance Storable e => Storable (Color CMYK e) where
  sizeOf = sizeOfN 4
  {-# INLINE sizeOf #-}
  alignment = alignmentN 4
  {-# INLINE alignment #-}
  peek = peek4 ColorCMYK
  {-# INLINE peek #-}
  poke p (ColorCMYK c m y k) = poke4 p c m y k
  {-# INLINE poke #-}

cmyk2rgb :: (RealFloat e, Elevator e) => Color CMYK e -> Color RGB e
cmyk2rgb (ColorCMYK c m y k) = ColorRGB (clamp01 r) (clamp01 g) (clamp01 b)
  where
    !k' = maxValue - k
    !r = (maxValue - c) * k'
    !g = (maxValue - m) * k'
    !b = (maxValue - y) * k'
{-# INLINE cmyk2rgb #-}


rgb2cmyk :: (RealFloat e, Elevator e) => Color RGB e -> Color CMYK e
rgb2cmyk (ColorRGB r g b) = ColorCMYK c m y k
  where
    !c = (k' - r) / k'
    !m = (k' - g) / k'
    !y = (k' - b) / k'
    !k = maxValue - k'
    !k' = max r (max g b)
{-# INLINE rgb2cmyk #-}
