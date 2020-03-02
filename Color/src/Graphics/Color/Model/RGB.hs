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
  , pattern ColorRGB
  , pattern ColorRGBA
  , pattern RGB
  , Color
  , ColorModel(..)
  ) where

import Foreign.Storable
import Graphics.Color.Algebra
import Graphics.Color.Model.Internal

-- | The most common @RGB@ color model
data RGB


-- | `RGB` color model
newtype instance Color RGB e = RGB (V3 e)

-- | Constructor for @RGB@ with alpha channel.
pattern ColorRGB :: e -> e -> e -> Color RGB e
pattern ColorRGB r g b = RGB (V3 r g b)
{-# COMPLETE ColorRGB #-}

-- | Constructor for @RGB@ with alpha channel.
pattern ColorRGBA :: e -> e -> e -> e -> Color (Alpha RGB) e
pattern ColorRGBA r g b a = Alpha (RGB (V3 r g b)) a
{-# COMPLETE ColorRGBA #-}

-- | `RGB` color model
deriving instance Eq e => Eq (Color RGB e)
-- | `RGB` color model
deriving instance Ord e => Ord (Color RGB e)
-- | `RGB` color model
instance Elevator e => Show (Color RGB e) where
  showsPrec _ = showsColorModel

-- | `RGB` color model
instance Elevator e => ColorModel RGB e where
  type Components RGB e = (e, e, e)
  toComponents (ColorRGB r g b) = (r, g, b)
  {-# INLINE toComponents #-}
  fromComponents (r, g, b) = ColorRGB r g b
  {-# INLINE fromComponents #-}

-- | `RGB` color model
deriving instance Functor (Color RGB)
-- | `RGB` color model
deriving instance Applicative (Color RGB)
-- | `RGB` color model
deriving instance Foldable (Color RGB)
-- | `RGB` color model
deriving instance Traversable (Color RGB)
-- | `RGB` color model
deriving instance Storable e => Storable (Color RGB e)
