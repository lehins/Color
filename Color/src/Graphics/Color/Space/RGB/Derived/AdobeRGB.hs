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
-- Module      : Graphics.Color.Space.RGB.Derived.AdobeRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Derived.AdobeRGB
  ( AdobeRGB
  , AdobeRGB.primaries
  , AdobeRGB.transfer
  , AdobeRGB.itransfer
  , module Graphics.Color.Space
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import qualified Graphics.Color.Space.RGB.AdobeRGB as AdobeRGB


-- | The most common @AdobeRGB@ color space with an arbitrary illuminant
data AdobeRGB (i :: k)

-- | `AdobeRGB` color space (derived)
newtype instance Color (AdobeRGB i) e = AdobeRGB (Color CM.RGB e)

-- | `AdobeRGB` color space (derived)
deriving instance Eq e => Eq (Color (AdobeRGB i) e)
-- | `AdobeRGB` color space (derived)
deriving instance Ord e => Ord (Color (AdobeRGB i) e)
-- | `AdobeRGB` color space (derived)
deriving instance Functor (Color (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Applicative (Color (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Foldable (Color (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Traversable (Color (AdobeRGB i))
-- | `AdobeRGB` color space (derived)
deriving instance Storable e => Storable (Color (AdobeRGB i) e)

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => Show (Color (AdobeRGB (i :: k)) e) where
  showsPrec _ = showsColorModel

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorModel (AdobeRGB (i :: k)) e where
  type Components (AdobeRGB i) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `AdobeRGB` color space (derived)
instance (Illuminant i, Elevator e) => ColorSpace (AdobeRGB i) i e where
  type BaseModel (AdobeRGB i) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | `AdobeRGB` color space (derived)
instance Illuminant i => RedGreenBlue (AdobeRGB i) i where
  gamut = AdobeRGB.primaries
  ecctf = fmap AdobeRGB.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap AdobeRGB.itransfer
  {-# INLINE dcctf #-}
