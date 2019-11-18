{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Derived.AdobeRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Derived.AdobeRGB
  ( AdobeRGB
  , AdobeRGB.primaries
  , AdobeRGB.transfer
  , AdobeRGB.itransfer
  , module Graphics.ColorSpace
  , module Graphics.ColorSpace.CIE1931.Illuminant
  ) where

import Data.Proxy
import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace
import Graphics.ColorSpace.CIE1931.Illuminant
import qualified Graphics.ColorSpace.RGB.AdobeRGB as AdobeRGB


-- | The most common @AdobeRGB@ color space with an arbitrary illuminant
data AdobeRGB (i :: k)

-- | `AdobeRGB` color space (derived)
newtype instance Pixel (AdobeRGB i) e = AdobeRGB (Pixel CM.RGB e)

-- | `AdobeRGB` color space (derived)
deriving instance Eq e => Eq (Pixel (AdobeRGB i) e)
-- | `AdobeRGB` color space (derived)
deriving instance Ord e => Ord (Pixel (AdobeRGB i) e)
-- | `AdobeRGB` color space (derived)
deriving instance Functor (Pixel (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Applicative (Pixel (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Foldable (Pixel (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Traversable (Pixel (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Storable e => Storable (Pixel (AdobeRGB i) e)

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => Show (Pixel (AdobeRGB (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorModel (AdobeRGB (i :: k)) e where
  type Components (AdobeRGB i) e = (e, e, e)
  toComponents = toComponents . unPixelRGB
  {-# INLINE toComponents #-}
  fromComponents = mkPixelRGB . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (AdobeRGB (i :: k)) e where
  type BaseColorSpace (AdobeRGB i) = AdobeRGB i
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = showsType (Proxy :: Proxy (AdobeRGB i))

-- | `AdobeRGB` color space (derived)
instance Illuminant i => RedGreenBlue (AdobeRGB i) i where
  chromaticity = AdobeRGB.primaries
  ecctf = fmap AdobeRGB.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap AdobeRGB.itransfer
  {-# INLINE dcctf #-}
