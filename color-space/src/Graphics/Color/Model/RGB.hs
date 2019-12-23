{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Model.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model.RGB
  ( RGB
  -- * Constructors for an RGB color model.
  , pattern PixelRGB
  , pattern PixelRGBA
  , pattern RGB
  , Pixel
  , ColorModel(..)
  ) where

import Foreign.Storable
import Graphics.Color.Algebra
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal

-- | The most common @RGB@ color model
data RGB


-- | `RGB` color model
newtype instance Pixel RGB e = RGB (V3 e)

-- | Constructor for @RGB@ with alpha channel.
pattern PixelRGB :: e -> e -> e -> Pixel RGB e
pattern PixelRGB r g b = RGB (V3 r g b)
{-# COMPLETE PixelRGB #-}

-- | Constructor for @RGB@ with alpha channel.
pattern PixelRGBA :: e -> e -> e -> e -> Pixel (Alpha RGB) e
pattern PixelRGBA r g b a = Alpha (RGB (V3 r g b)) a
{-# COMPLETE PixelRGBA #-}

-- | `RGB` color model
deriving instance Eq e => Eq (Pixel RGB e)
-- | `RGB` color model
deriving instance Ord e => Ord (Pixel RGB e)
-- | `RGB` color model
instance Elevator e => Show (Pixel RGB e) where
  showsPrec _ = showsColorModel

-- | `RGB` color model
instance Elevator e => ColorModel RGB e where
  type Components RGB e = (e, e, e)
  toComponents (PixelRGB r g b) = (r, g, b)
  {-# INLINE toComponents #-}
  fromComponents (r, g, b) = PixelRGB r g b
  {-# INLINE fromComponents #-}

-- | `RGB` color model
deriving instance Functor (Pixel RGB)
-- | `RGB` color model
deriving instance Applicative (Pixel RGB)
-- | `RGB` color model
deriving instance Foldable (Pixel RGB)
-- | `RGB` color model
deriving instance Traversable (Pixel RGB)
-- | `RGB` color model
deriving instance Storable e => Storable (Pixel RGB e)
