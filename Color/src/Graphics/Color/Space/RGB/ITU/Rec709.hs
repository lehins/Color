{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.ITU.Rec709
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.ITU.Rec709
  ( pattern BT709
  , BT709
  , D65
  ) where

import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import Graphics.Color.Space.RGB.ITU.Rec601 as Rec601 (D65, BT601_625)
import Graphics.Color.Space.RGB.Luma

-- | [ITU-R BT.709](https://en.wikipedia.org/wiki/Rec._709) color space
data BT709 (l :: Linearity)

newtype instance Color (BT709 l) e = BT709 (Color CM.RGB e)

-- | ITU-R BT.709 color space
deriving instance Eq e => Eq (Color (BT709 l) e)
-- | ITU-R BT.709 color space
deriving instance Ord e => Ord (Color (BT709 l) e)
-- | ITU-R BT.709 color space
deriving instance Functor (Color (BT709 l))
-- | ITU-R BT.709 color space
deriving instance Applicative (Color (BT709 l))
-- | ITU-R BT.709 color space
deriving instance Foldable (Color (BT709 l))
-- | ITU-R BT.709 color space
deriving instance Traversable (Color (BT709 l))
-- | ITU-R BT.709 color space
deriving instance Storable e => Storable (Color (BT709 l) e)

-- | ITU-R BT.709 color space
instance (Typeable l, Elevator e) => Show (Color (BT709 l) e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.709 color space
instance (Typeable l, Elevator e) => ColorModel (BT709 l) e where
  type Components (BT709 l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | ITU-R BT.709 linear color space
instance Elevator e => ColorSpace (BT709 'Linear) D65 e where
  type BaseModel (BT709 'Linear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  grayscale = rgbLinearGrayscale
  {-# INLINE grayscale #-}
  applyGrayscale = rgbLinearApplyGrayscale
  {-# INLINE applyGrayscale #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgbLinear
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.709 color space
instance Elevator e => ColorSpace (BT709 'NonLinear) D65 e where
  type BaseModel (BT709 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  grayscale = rgbNonLinearGrayscale
  {-# INLINE grayscale #-}
  applyGrayscale = rgbNonLinearApplyGrayscale
  {-# INLINE applyGrayscale #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.709 color space
instance RedGreenBlue BT709 D65 where
  gamut = Gamut (Primary 0.64 0.33)
                (Primary 0.30 0.60)
                (Primary 0.15 0.06)
  transfer = transfer @_ @BT601_625
  {-# INLINE transfer #-}
  itransfer = itransfer @_ @BT601_625
  {-# INLINE itransfer #-}


instance Luma BT709 where
  rWeight = 0.2126
  gWeight = 0.7152
  bWeight = 0.0722
