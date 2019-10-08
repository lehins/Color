{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.SRGB
  ( SRGB
  , pattern PixelRGB
  , pattern PixelRGBA
  , RGB
  , primaries
  , npmStandard
  , inpmStandard
  , transfer
  , itransfer
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Alpha
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.CIE1931.Illuminants
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal


-- | The most common @sRGB@ color space with the default `D65` illuminant
type SRGB = RGB 'D65

-- | The most common @sRGB@ color space
data RGB (i :: Illuminant2)

newtype instance Pixel (RGB 'D65) e = RGB (Pixel CM.RGB e)

-- | Constructor for the most common @sRGB@ color space with the default `D65` illuminant
pattern PixelRGB :: e -> e -> e -> Pixel SRGB e
pattern PixelRGB r g b = RGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}

-- | Constructor for @sRGB@ with alpha channel.
pattern PixelRGBA :: e -> e -> e -> e -> Pixel (Alpha SRGB) e
pattern PixelRGBA r g b a = Alpha (RGB (CM.PixelRGB r g b)) a
{-# COMPLETE PixelRGBA #-}


-- | s`RGB` color space
deriving instance Eq e => Eq (Pixel (RGB 'D65) e)
-- | s`RGB` color space
deriving instance Ord e => Ord (Pixel (RGB 'D65) e)
-- | s`RGB` color space
deriving instance Functor (Pixel (RGB 'D65))
-- | s`RGB` color space
deriving instance Applicative (Pixel (RGB 'D65))
-- | s`RGB` color space
deriving instance Foldable (Pixel (RGB 'D65))
-- | s`RGB` color space
deriving instance Traversable (Pixel (RGB 'D65))
-- | s`RGB` color space
deriving instance Storable e => Storable (Pixel (RGB 'D65) e)

-- | s`RGB` color space
instance Elevator e => Show (Pixel (RGB 'D65) e) where
  showsPrec _ = showsColorModel

-- | s`RGB` color space
instance Elevator e => ColorModel (RGB 'D65) e where
  type Components (RGB 'D65) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | s`RGB` color space
instance Elevator e => ColorSpace (RGB 'D65) e where
  type BaseColorSpace (RGB 'D65) = RGB 'D65
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = ("sRGB Standard" ++)

-- | s`RGB` color space
instance RedGreenBlue RGB 'D65 where
  chromaticity = primaries
  npm = npmStandard
  inpm = inpmStandard
  ecctf = fmap transfer
  {-# INLINE ecctf #-}
  dcctf = fmap itransfer
  {-# INLINE dcctf #-}


-- | sRGB normalized primary matrix. This is a helper definition, use `npm` instead.
--
-- >>> npmStandard
-- [ [ 0.4124000, 0.3576000, 0.1805000]
-- , [ 0.2126000, 0.7152000, 0.0722000]
-- , [ 0.0193000, 0.1192000, 0.9505000] ]
--
-- @since 0.1.0
npmStandard :: NPM RGB 'D65
npmStandard = NPM $ M3x3 (V3 0.4124 0.3576 0.1805)
                         (V3 0.2126 0.7152 0.0722)
                         (V3 0.0193 0.1192 0.9505)


-- | sRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
--
-- >>> inpmStandard
-- [ [ 3.2406000,-1.5372000,-0.4986000]
-- , [-0.9689000, 1.8758000, 0.0415000]
-- , [ 0.0557000,-0.2040000, 1.0570000] ]
--
-- @since 0.1.0
inpmStandard :: INPM RGB 'D65
inpmStandard = INPM $ M3x3 (V3  3.2406 -1.5372 -0.4986)
                           (V3 -0.9689  1.8758  0.0415)
                           (V3  0.0557 -0.2040  1.0570)


-- | sRGB transfer function "gamma". This is a helper function, therefore `ecctf` should be used
-- instead.
--
-- \[
-- \gamma(u) = \begin{cases}
--     12.92 u & u \leq 0.0031308 \\
--     1.055 u^{1/2.4} - 0.055 & \text{otherwise}
--   \end{cases}
-- \]
--
-- @since 0.1.0
transfer :: Elevator e => Double -> e
transfer u
  | u <= 0.0031308 = fromDouble (12.92 * u)
  | otherwise = fromDouble (1.055 * (u ** (1 / 2.4)) - 0.055)
{-# INLINE transfer #-}

-- | sRGB inverse transfer function "gamma". This is a helper function, therefore `dcctf` should
-- be used instead.
--
-- \[
-- \gamma^{-1}(u) = \begin{cases}
--     u / 12.92 & u \leq 0.04045 \\
--     \left(\tfrac{u + 0.055}{1.055}\right)^{2.4} & \text{otherwise}
--   \end{cases}
-- \]
--
-- @since 0.1.0
itransfer :: Elevator e => e -> Double
itransfer eu
  | u <= 0.04045 = u / 12.92
  | otherwise = ((u + 0.055) / 1.055) ** 2.4
  where !u = toDouble eu
{-# INLINE itransfer #-}


-- | Primaries for sRGB color space. This is a helper definition, therefore `chromaticity` should
-- be used instead.
--
-- @since 0.1.0
primaries :: Illuminant i => Chromaticity rgb i
primaries = Chromaticity (Primary 0.64 0.33)
                         (Primary 0.30 0.60)
                         (Primary 0.15 0.06)
