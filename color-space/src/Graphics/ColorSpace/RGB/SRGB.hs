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
  , ITU(..)
  , pattern PixelRGB
  , pattern PixelRGBA
  , ToRGB(..)
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
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal
import Graphics.ColorSpace.ITU
import Graphics.ColorSpace.ITU.Rec709 (primaries)
import Graphics.ColorSpace.YUV.YCbCr


-- | The most common @sRGB@ color space with the default `D65` illuminant
type SRGB = RGB 'D65

-- | The most common @sRGB@ color space
data RGB (i :: ITU)

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
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
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
-- [ [ 0.412400, 0.357600, 0.180500 ]
-- , [ 0.212600, 0.715200, 0.072200 ]
-- , [ 0.019300, 0.119200, 0.950500 ] ]
--
-- @since 0.1.0
npmStandard :: RealFloat a => NPM RGB 'D65 a
npmStandard = NPM $ M3x3 (V3 0.4124 0.3576 0.1805)
                         (V3 0.2126 0.7152 0.0722)
                         (V3 0.0193 0.1192 0.9505)


-- | sRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
--
-- >>> inpmStandard
-- [ [ 3.240600,-1.537200,-0.498600 ]
-- , [-0.968900, 1.875800, 0.041500 ]
-- , [ 0.055700,-0.204000, 1.057000 ] ]
--
-- @since 0.1.0
inpmStandard :: RealFloat a => INPM RGB 'D65 a
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
transfer :: (Ord a, Floating a) => a -> a
transfer u
  | u <= 0.0031308 = 12.92 * u
  | otherwise = 1.055 * (u ** (1 / 2.4)) - 0.055
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
itransfer :: (Ord a, Floating a) => a -> a
itransfer u
  | u <= 0.04045 = u / 12.92
  | otherwise = ((u + 0.055) / 1.055) ** 2.4
{-# INLINE itransfer #-}

-- | Conversion to s`RGB` color model.
class ToRGB cs where

  -- | Convert to an s`RGB` pixel.
  toPixelRGB :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel SRGB a
  -- | Convert to an s`RGB` pixel with alpha channel
  toPixelRGBA :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel (Alpha SRGB) a
  toPixelRGBA = (`addAlpha` 1) . toPixelRGB
  {-# INLINE toPixelRGBA #-}

instance ToRGB SRGB where
  toPixelRGB = fmap toRealFloat
  {-# INLINE toPixelRGB #-}

instance ToRGB YCbCr where
  toPixelRGB = ycbcr2rgb . fmap toRealFloat
  {-# INLINE toPixelRGB #-}


instance ToYCbCr SRGB where
  toPixelYCbCr = rgb2ycbcr . fmap toRealFloat
  {-# INLINE toPixelYCbCr #-}


ycbcr2rgb :: Fractional e => Pixel YCbCr e -> Pixel SRGB e
ycbcr2rgb (PixelYCbCr y cb cr) = PixelRGB r g b
  where
    !cb05 = cb - 0.5
    !cr05 = cr - 0.5
    !r = y                  +   1.402 * cr05
    !g = y - 0.34414 * cb05 - 0.71414 * cr05
    !b = y +   1.772 * cb05

rgb2ycbcr :: Fractional e => Pixel SRGB e -> Pixel YCbCr e
rgb2ycbcr (PixelRGB r g b) = PixelYCbCr y cb cr
  where
    !y  =          0.299 * r +    0.587 * g +    0.114 * b
    !cb = 0.5 - 0.168736 * r - 0.331264 * g +      0.5 * b
    !cr = 0.5 +      0.5 * r - 0.418688 * g - 0.081312 * b
{-# INLINE rgb2ycbcr #-}
