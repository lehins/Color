{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.SRGB
  ( -- * Constructors for a sRGB color space.
    pattern SRGB
  , pattern ColorSRGB
  , pattern ColorSRGBA
  , SRGB
  , D50
  , D65
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Illuminant.ICC.PCS (D50)
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import Graphics.Color.Space.RGB.ITU.Rec601 (applyGrayscaleRec601)
import Graphics.Color.Space.RGB.ITU.Rec709 (BT709, D65)
import Graphics.Color.Space.RGB.Luma

-- | The most common [sRGB](https://en.wikipedia.org/wiki/SRGB) color space, which is
-- defined with:
--
-- * `D65` illuminant
--
-- * Transfer function:
--
-- \[
-- \gamma(u) = \begin{cases}
--     12.92 u & u \leq 0.0031308 \\
--     1.055 u^{1/2.4} - 0.055 & \text{otherwise}
--   \end{cases}
-- \]
--
-- * Inverse transfer function
--
-- \[
-- \gamma^{-1}(u) = \begin{cases}
--     u / 12.92 & u \leq 0.04045 \\
--     \left(\tfrac{u + 0.055}{1.055}\right)^{2.4} & \text{otherwise}
--   \end{cases}
-- \]
--
-- * Normalized primary matrix:
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB
-- >>> npm :: NPM SRGB Float
-- [ [ 0.41240000, 0.35760000, 0.18050000 ]
-- , [ 0.21260000, 0.71520000, 0.07220000 ]
-- , [ 0.01930000, 0.11920000, 0.95050000 ] ]
--
-- * Inverse normalized primary matrix:
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB
-- >>> inpm :: INPM SRGB Float
-- [ [ 3.24060000,-1.53720000,-0.49860000 ]
-- , [-0.96890000, 1.87580000, 0.04150000 ]
-- , [ 0.05570000,-0.20400000, 1.05700000 ] ]
--
data SRGB (l :: Linearity)


newtype instance Color (SRGB l) e = SRGB (Color CM.RGB e)

-- | Constructor for a color in @sRGB@ color space
--
-- @since 0.1.0
pattern ColorSRGB :: e -> e -> e -> Color (SRGB l) e
pattern ColorSRGB r g b = SRGB (CM.ColorRGB r g b)
{-# COMPLETE ColorSRGB #-}

-- | Constructor for a color in @sRGB@ color space with alphs channel
--
-- @since 0.1.0
pattern ColorSRGBA :: e -> e -> e -> e -> Color (Alpha (SRGB l)) e
pattern ColorSRGBA r g b a = Alpha (SRGB (CM.ColorRGB r g b)) a
{-# COMPLETE ColorSRGBA #-}


-- | `SRGB` color space
deriving instance Eq e => Eq (Color (SRGB l) e)
-- | `SRGB` color space
deriving instance Ord e => Ord (Color (SRGB l) e)
-- | `SRGB` color space
deriving instance Functor (Color (SRGB l))
-- | `SRGB` color space
deriving instance Applicative (Color (SRGB l))
-- | `SRGB` color space
deriving instance Foldable (Color (SRGB l))
-- | `SRGB` color space
deriving instance Traversable (Color (SRGB l))
-- | `SRGB` color space
deriving instance Storable e => Storable (Color (SRGB l) e)

-- | `SRGB` color space
instance (Typeable l, Elevator e) => Show (Color (SRGB l) e) where
  showsPrec _ = showsColorModel

-- | `SRGB` color space
instance (Typeable l, Elevator e) => ColorModel (SRGB l) e where
  type Components (SRGB l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `SRGB` linear color space
instance Elevator e => ColorSpace (SRGB 'Linear) D65 e where
  type BaseModel (SRGB 'Linear) = CM.RGB
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


-- | `SRGB` linear color space
instance Elevator e => ColorSpace (SRGB 'NonLinear) D65 e where
  type BaseModel (SRGB 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  grayscale = fmap fromDouble . coerce . rgbLuma @_ @_ @_ @Double
  {-# INLINE grayscale #-}
  applyGrayscale = applyGrayscaleRec601
  {-# INLINE applyGrayscale #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | `SRGB` color space
instance RedGreenBlue SRGB D65 where
  gamut = coerce (gamut @_ @BT709)
  npm = NPM $ M3x3 (V3 0.4124 0.3576 0.1805)
                   (V3 0.2126 0.7152 0.0722)
                   (V3 0.0193 0.1192 0.9505)
  inpm = INPM $ M3x3 (V3  3.2406 -1.5372 -0.4986)
                     (V3 -0.9689  1.8758  0.0415)
                     (V3  0.0557 -0.2040  1.0570)
  transfer u
    | u <= 0.0031308 = 12.92 * u
    | otherwise = 1.055 * (u ** (1 / 2.4)) - 0.055
  {-# INLINE transfer #-}
  itransfer u
    | u <= 0.04045 = u / 12.92
    | otherwise = ((u + 0.055) / 1.055) ** 2.4
  {-# INLINE itransfer #-}

instance Luma SRGB where
  rWeight = 0.299
  gWeight = 0.587
  bWeight = 0.114
