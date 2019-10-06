{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RedGreenBlue.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RedGreenBlue.Internal
  ( RedGreenBlue(..)
  , rgb2xyz
  , xyz2rgb
  , NPM(..)
  , npmDerive
  , INPM(..)
  , inpmDerive
  , pixelChromaticity
  , chromaticityWhitePoint
  ) where

import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Data.Coerce

class Illuminant i => RedGreenBlue (cs :: k -> *) (i :: k) where
  chromaticity :: Chromaticity cs i

  -- | Encoding color component transfer function (inverse)
  ecctf :: Elevator e => Pixel (cs i) Double -> Pixel (cs i) e

  -- | Decoding color component transfer function (forward)
  dcctf :: Elevator e => Pixel (cs i) e -> Pixel (cs i) Double

  mkPixelRGB :: Pixel CM.RGB e -> Pixel (cs i) e
  default mkPixelRGB ::
    Coercible (Pixel CM.RGB e) (Pixel (cs i) e) => Pixel CM.RGB e -> Pixel (cs i) e
  mkPixelRGB = coerce

  unPixelRGB :: Pixel (cs i) e -> Pixel CM.RGB e
  default unPixelRGB ::
    Coercible (Pixel (cs i) e) (Pixel CM.RGB e) => Pixel (cs i) e -> Pixel CM.RGB e
  unPixelRGB = coerce

  npm :: NPM cs i
  npm = npmDerive chromaticity

  inpm :: INPM cs i
  inpm = inpmDerive chromaticity

  npmApply :: Pixel (cs i) Double -> Pixel XYZ Double
  npmApply px = fromV3 PixelXYZ (multM3x3byV3 (unNPM (npm :: NPM cs i)) (toV3 r g b))
    where CM.PixelRGB r g b = unPixelRGB px
  {-# INLINE npmApply #-}

  inpmApply :: Pixel XYZ Double -> Pixel (cs i) Double
  inpmApply (PixelXYZ x y z) =
    mkPixelRGB $ fromV3 CM.PixelRGB (multM3x3byV3 (unINPM (inpm :: INPM cs i)) (toV3 x y z))
  {-# INLINE inpmApply #-}


rgb2xyz :: (RedGreenBlue cs i, Elevator e) => Pixel (cs i) e -> Pixel XYZ Double
rgb2xyz = npmApply . dcctf

xyz2rgb :: (RedGreenBlue cs i, Elevator e) => Pixel XYZ Double -> Pixel (cs i) e
xyz2rgb = ecctf . inpmApply


-- newtype ConversionMatrix cs' cs = ConversionMatrix M3x3

-- conversionMatrix ::
--      forall cs' i' cs i. (RedGreenBlue cs' i', RedGreenBlue cs i)
--   => ConversionMatrix (cs' i') (cs i)
-- conversionMatrix =
--   ConversionMatrix $ multM3x3byM3x3 (unINPM (inpm :: INPM cs i)) (unNPM (npm :: NPM cs' i'))

-- makeConversionMatrix ::
--      forall cta cs' i' cs i. (RedGreenBlue cs' i', RedGreenBlue cs i)
--   => cta -> ConversionMatrix (cs' i') (cs i)
-- makeConversionMatrix _cta =
--   ConversionMatrix $ multM3x3byM3x3 (unINPM (inpm :: INPM cs i)) (unNPM (npm :: NPM cs' i'))


-- applyConversionMatrix ::
--      (RedGreenBlue cs1 i1, RedGreenBlue cs2 i2, Elevator e2, Elevator e1)
--   => ConversionMatrix (cs1 i1) (cs2 i2)
--   -> Pixel (cs1 i1) e1
--   -> Pixel (cs2 i2) e2
-- applyConversionMatrix (ConversionMatrix m) px =
--   mkPixelRGB $ fromV3 CM.PixelRGB $ multM3x3byV3 m (toV3 r g b)
--   where
--     CM.PixelRGB r g b = unPixelRGB px

-- | Normalized primary matrix (NPM), which is used to tranform linear RGB color space
-- into `Graphics.ColorSpace.CIE1931.XYZ.XYZ` color space.
--
-- @since 0.1.0
newtype NPM (cs :: k -> *) (i :: k) = NPM
  { unNPM :: M3x3
  } deriving (Eq)

instance Show (NPM cs i) where
  show = show . unNPM

-- | Inverse normalized primary matrix (iNPM), which is used to tranform linear
-- `Graphics.ColorSpace.CIE1931.XYZ.XYZ` color space into an RGB color space. It is
-- literally a matrix inverse of `NPM`
--
-- @since 0.1.0
newtype INPM (cs :: k -> *) (i :: k) = INPM
  { unINPM :: M3x3
  } deriving (Eq)

instance Show (INPM cs i) where
  show = show . unINPM


-- | Derive a `NPM` form chromaticities and a white point
--
-- @since 0.1.0
npmDerive :: forall cs i . Illuminant i => Chromaticity cs i -> NPM cs i
npmDerive (Chromaticity r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
  where
    -- transposed matrix with xyz primaries
    !primaries' = M3x3 (V3 (xPrimary r) (xPrimary g) (xPrimary b))
                       (V3 (yPrimary r) (yPrimary g) (yPrimary b))
                       (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ (whitePoint :: WhitePoint i)
{-# INLINE npmDerive #-}

-- | Derive an `INPM` form chromaticities and a white point
--
-- @since 0.1.0
inpmDerive :: forall cs i . Illuminant i => Chromaticity cs i -> INPM cs i
inpmDerive = coerce . invertM3x3 . coerce . npmDerive
{-# INLINE inpmDerive #-}


-- | Get the `WhitePoint` of chromaticity. `Chromaticity` itself isn't actually evaluated,
-- its type carries enough information for this operation.
--
-- @since 0.1.0
chromaticityWhitePoint :: RedGreenBlue cs i => Chromaticity cs i -> WhitePoint i
chromaticityWhitePoint _ = whitePoint
{-# INLINE chromaticityWhitePoint #-}

-- | Get the `Chromaticity` of a pixel in RGB color space. Pixel itself isn't actually
-- evaluated, its type carries enough information for this operation.
--
-- @since 0.1.0
pixelChromaticity :: RedGreenBlue cs i => Pixel (cs i) e -> Chromaticity cs i
pixelChromaticity _ = chromaticity
{-# INLINE pixelChromaticity #-}

