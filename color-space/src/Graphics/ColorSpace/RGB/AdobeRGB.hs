{-# LANGUAGE StandaloneDeriving #-}
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
-- Module      : Graphics.ColorSpace.RGB.AdobeRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.AdobeRGB
  ( AdobeRGB
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
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal
import Graphics.ColorSpace.ITU


-- | A very common @AdobeRGB@ color space with the default `D65` illuminant
type AdobeRGB = RGB 'D65

-- | A very common @AdobeRGB@ color space
data RGB (i :: ITU)

-- | Constructor for a very common @AdobeRGB@ color space with the default `D65` illuminant
pattern PixelRGB :: e -> e -> e -> Pixel AdobeRGB e
pattern PixelRGB r g b = RGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}

-- | Constructor for @AdobeRGB@ with alpha channel.
pattern PixelRGBA :: e -> e -> e -> e -> Pixel (Alpha AdobeRGB) e
pattern PixelRGBA r g b a = Alpha (RGB (CM.PixelRGB r g b)) a
{-# COMPLETE PixelRGBA #-}

newtype instance Pixel (RGB 'D65) e = RGB (Pixel CM.RGB e)

-- | Adobe`RGB` color space
deriving instance Eq e => Eq (Pixel (RGB 'D65) e)
-- | Adobe`RGB` color space
deriving instance Ord e => Ord (Pixel (RGB 'D65) e)
-- | Adobe`RGB` color space
deriving instance Functor (Pixel (RGB 'D65))
-- | Adobe`RGB` color space
deriving instance Applicative (Pixel (RGB 'D65))
-- | Adobe`RGB` color space
deriving instance Foldable (Pixel (RGB 'D65))
-- | Adobe`RGB` color space
deriving instance Traversable (Pixel (RGB 'D65))
-- | Adobe`RGB` color space
deriving instance Storable e => Storable (Pixel (RGB 'D65) e)

-- | Adobe`RGB` color space
instance Elevator e => Show (Pixel (RGB 'D65) e) where
  showsPrec _ = showsColorModel

-- | Adobe`RGB` color space
instance Elevator e => ColorModel (RGB 'D65) e where
  type Components (RGB 'D65) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | Adobe`RGB` color space
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
  showsColorSpaceName _ = ("AdobeRGB 1998 Standard" ++)


-- | Adobe`RGB` color space
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
-- [ [ 0.576670, 0.185560, 0.188230 ]
-- , [ 0.297340, 0.627360, 0.075290 ]
-- , [ 0.027030, 0.070690, 0.991340 ] ]
--
-- @since 0.1.0
npmStandard :: RealFloat a => NPM RGB 'D65 a
npmStandard = NPM $ M3x3 (V3 0.57667 0.18556 0.18823)
                         (V3 0.29734 0.62736 0.07529)
                         (V3 0.02703 0.07069 0.99134)


-- | sRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
--
-- >>> inpmStandard
-- [ [ 2.041590,-0.565010,-0.344730 ]
-- , [-0.969240, 1.875970, 0.041560 ]
-- , [ 0.013440,-0.118360, 1.015170 ] ]
--
-- @since 0.1.0
inpmStandard :: RealFloat a => INPM RGB 'D65 a
inpmStandard = INPM $ M3x3 (V3  2.04159 -0.56501 -0.34473)
                           (V3 -0.96924  1.87597  0.04156)
                           (V3  0.01344 -0.11836  1.01517)



-- | AdobeRGB transfer function "gamma":
--
-- \[
-- \gamma(u) = u^{2.19921875} = u^\frac{563}{256}
-- \]
--
-- @since 0.1.0
transfer :: Floating a => a -> a
transfer u = u ** (256 / 563)
{-# INLINE transfer #-}


-- | AdobeRGB inverse transfer function "gamma":
--
-- \[
-- \gamma^{-1}(u) = u^\frac{1}{2.19921875} = u^\frac{256}{563}
-- \]
--
-- @since 0.1.0
itransfer :: Floating a => a -> a
itransfer u = u ** 2.19921875 -- in rational form 563/256
{-# INLINE itransfer #-}

primaries :: Illuminant i => Chromaticity rgb i
primaries = Chromaticity (Primary 0.64 0.33)
                         (Primary 0.21 0.71)
                         (Primary 0.15 0.06)
