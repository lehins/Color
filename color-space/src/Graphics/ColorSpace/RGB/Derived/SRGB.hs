{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Derived.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Derived.SRGB
  ( pattern PixelRGB
  , RGB
  , SRGB.primaries
  , SRGB.transfer
  , SRGB.itransfer
  ) where

import Data.Proxy
import Data.Coerce
import Foreign.Storable
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import qualified Graphics.ColorSpace.RGB.SRGB as SRGB
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal


-- | The most common @sRGB@ color space with an arbitrary illuminant
data RGB (i :: k)

-- | s`RGB` color space (derived)
newtype instance Pixel (RGB i) e = RGB (Pixel CM.RGB e)

-- | Constructor for @sRGB@ color space with an arbitrary illuminant
pattern PixelRGB :: e -> e -> e -> Pixel (RGB i) e
pattern PixelRGB r g b = RGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}


-- | s`RGB` color space (derived)
deriving instance Eq e => Eq (Pixel (RGB i) e)
-- | s`RGB` color space (derived)
deriving instance Ord e => Ord (Pixel (RGB i) e)
-- | s`RGB` color space (derived)
deriving instance Functor (Pixel (RGB i))
-- | s`RGB` color space (derived)
deriving instance Applicative (Pixel (RGB i))
-- | s`RGB` color space (derived)
deriving instance Foldable (Pixel (RGB i))
-- | s`RGB` color space (derived)
deriving instance Traversable (Pixel (RGB i))
-- | s`RGB` color space (derived)
deriving instance Storable e => Storable (Pixel (RGB i) e)

-- | s`RGB` color space (derived)
instance (Illuminant i, Elevator e) => Show (Pixel (RGB (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | s`RGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorModel (RGB (i :: k)) e where
  type Components (RGB i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | s`RGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (RGB (i :: k)) e where
  type BaseColorSpace (RGB i) = RGB i
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = ('s':) . showsType (Proxy :: Proxy (RGB i))

-- | s`RGB` color space (derived)
instance Illuminant i => RedGreenBlue RGB i where
  chromaticity = SRGB.primaries
  ecctf = fmap SRGB.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap SRGB.itransfer
  {-# INLINE dcctf #-}
