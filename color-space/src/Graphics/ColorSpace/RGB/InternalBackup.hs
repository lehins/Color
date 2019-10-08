{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Internal
  ( StandardRGB(..)
  , standardRGBtoXYZ
  , standardXYZtoRGB
  , standardLinearRGBtoXYZ
  , standardLinearXYZtoRGB
  , DerivedRGB(..)
  , derivedRGBtoXYZ
  , derivedXYZtoRGB
  , derivedLinearRGBtoXYZ
  , derivedLinearXYZtoRGB
  , NPM(..)
  , npmDerive
  , npmApply
  , INPM(..)
  , inpmDerive
  , inpmApply
  , pixelChromaticity
  , chromaticityWhitePoint
  ) where

import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Prelude hiding (map)
import Data.Coerce
--import GHC.TypeLits

-- class Illuminant i => LinearRGB (cs :: k -> *) (i :: k) where
--   chromaticity :: Chromaticity cs i

--   mkPixelRGB :: Pixel CM.RGB e -> Pixel (cs i) e

--   unPixelRGB :: Pixel (cs i) e -> Pixel CM.RGB e

--   npm :: NPM cs i
--   npm = npmDerive chromaticity

--   inpm :: INPM cs i
--   inpm = inpmDerive chromaticity

--   -- | Encoding color component transfer function (inverse)
--   ecctf :: ColorModel (cs i) e => Pixel CM.RGB Double -> Pixel (cs i) e
--   ecctf = mkPixelRGB . fmap fromDouble
--   {-# INLINE ecctf #-}

--   -- | Decoding color component transfer function (forward)
--   dcctf :: ColorModel (cs i) e => Pixel (cs i) e -> Pixel CM.RGB Double
--   dcctf = fmap toDouble . unPixelRGB
--   {-# INLINE dcctf #-}



--   npmApply :: Pixel (cs i) Double -> Pixel XYZ Double
--   npmApply px = fromV3 PixelXYZ (multM3x3byV3 (unNPM npm) (toV3 r g b))
--     where CM.PixelRGB r g b = unPixelRGB px
--   {-# INLINE npmApply #-}

  -- inpmApply :: (Elevator e1, Elevator e2) => INPM cs i -> Pixel XYZ e2 -> Pixel CM.RGB e1
  -- inpmApply inpm' (PixelXYZ x y z) = fromV3 CM.PixelRGB (multM3x3byV3 (unINPM inpm') (toV3 x y z))
  -- {-# INLINE inpmApply #-}


  -- standardRGBtoXYZ ::
  --      forall cs i e. (StandardRGB cs i, ColorModel (cs i) e)
  --   => Pixel (cs i) e
  --   -> Pixel XYZ Double
  -- standardRGBtoXYZ = npmApply (npmStandard :: NPM cs i) . dcctf
  -- {-# INLINE standardRGBtoXYZ #-}

  -- -- | Standard  transformation of @CIE1931 `XYZ`@ to @RGB@ colorspace.
  -- standardXYZtoRGB ::
  --      forall cs i e. (StandardRGB cs i, ColorModel (cs i) e)
  --   => Pixel XYZ Double
  --   -> Pixel (cs i) e
  -- standardXYZtoRGB = ecctf . inpmApply (inpmStandard :: INPM cs i)
  -- {-# INLINE standardXYZtoRGB #-}


class Illuminant i => DerivedRGB (cs :: k -> *) (i :: k) where
  chromaticity :: Chromaticity cs i

  mkPixelRGB :: Pixel CM.RGB e -> Pixel (cs i) e

  unPixelRGB :: Pixel (cs i) e -> Pixel CM.RGB e

  npmDerived :: NPM cs i
  npmDerived = npmDerive chromaticity

  inpmDerived :: INPM cs i
  inpmDerived = inpmDerive chromaticity

  -- | Encoding color component transfer function (inverse)
  ecctf :: ColorModel (cs i) e => Pixel CM.RGB Double -> Pixel (cs i) e
  ecctf = mkPixelRGB . fmap fromDouble
  {-# INLINE ecctf #-}

  -- | Decoding color component transfer function (forward)
  dcctf :: ColorModel (cs i) e => Pixel (cs i) e -> Pixel CM.RGB Double
  dcctf = fmap toDouble . unPixelRGB
  {-# INLINE dcctf #-}


class DerivedRGB cs i => StandardRGB cs i where

  npmStandard :: NPM cs i

  inpmStandard :: INPM cs i

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


npmDerive :: forall cs i . Illuminant i => Chromaticity cs i -> NPM cs i
npmDerive (Chromaticity r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
  where
    -- transposed matrix with xyz primaries
    !primaries' = M3x3 (V3 (xPrimary r) (xPrimary g) (xPrimary b))
                       (V3 (yPrimary r) (yPrimary g) (yPrimary b))
                       (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ (whitePoint :: WhitePoint i)
{-# INLINE npmDerive #-}

inpmDerive :: forall cs i . Illuminant i => Chromaticity cs i -> INPM cs i
inpmDerive = coerce . invertM3x3 . coerce . npmDerive
{-# INLINE inpmDerive #-}


npmApply :: (Elevator e1, Elevator e2) => NPM cs i -> Pixel CM.RGB e2 -> Pixel XYZ e1
npmApply npm' (CM.PixelRGB r g b) = fromV3 PixelXYZ (multM3x3byV3 (unNPM npm') (toV3 r g b))
{-# INLINE npmApply #-}

inpmApply :: (Elevator e1, Elevator e2) => INPM cs i -> Pixel XYZ e2 -> Pixel CM.RGB e1
inpmApply inpm' (PixelXYZ x y z) = fromV3 CM.PixelRGB (multM3x3byV3 (unINPM inpm') (toV3 x y z))
{-# INLINE inpmApply #-}

-- | Standard transformation of @RGB@ to @CIE1931 `XYZ`@ colorspace.
standardRGBtoXYZ ::
     forall cs i e. (StandardRGB cs i, ColorModel (cs i) e)
  => Pixel (cs i) e
  -> Pixel XYZ Double
standardRGBtoXYZ = npmApply (npmStandard :: NPM cs i) . dcctf
{-# INLINE standardRGBtoXYZ #-}

-- | Standard  transformation of @CIE1931 `XYZ`@ to @RGB@ colorspace.
standardXYZtoRGB ::
     forall cs i e. (StandardRGB cs i, ColorModel (cs i) e)
  => Pixel XYZ Double
  -> Pixel (cs i) e
standardXYZtoRGB = ecctf . inpmApply (inpmStandard :: INPM cs i)
{-# INLINE standardXYZtoRGB #-}


-- | Derived transformation of @RGB@ to @CIE1931 `XYZ`@ colorspace.
derivedRGBtoXYZ ::
     forall cs i e. (DerivedRGB cs i, ColorModel (cs i) e)
  => Pixel (cs i) e
  -> Pixel XYZ Double
derivedRGBtoXYZ = npmApply (npmDerived :: NPM cs i) . dcctf
{-# INLINE derivedRGBtoXYZ #-}

-- | Derived  transformation of @CIE1931 `XYZ`@ to @RGB@ colorspace.
derivedXYZtoRGB ::
     forall cs i e. (DerivedRGB cs i, ColorModel (cs i) e)
  => Pixel XYZ Double
  -> Pixel (cs i) e
derivedXYZtoRGB = ecctf . inpmApply (inpmDerived :: INPM cs i)
{-# INLINE derivedXYZtoRGB #-}



-- | Standard linear transformation of @RGB@ to @CIE1931 `XYZ`@ colorspace.
standardLinearRGBtoXYZ ::
     forall cs i e. (StandardRGB cs i, Elevator e)
  => Pixel (cs i) e
  -> Pixel XYZ Double
standardLinearRGBtoXYZ = npmApply (npmStandard :: NPM cs i) . unPixelRGB
{-# INLINE standardLinearRGBtoXYZ #-}

-- | Standard linear transformation of @CIE1931 `XYZ`@ to @RGB@ colorspace.
standardLinearXYZtoRGB ::
     forall cs i e. (StandardRGB cs i, Elevator e)
  => Pixel XYZ Double
  -> Pixel (cs i) e
standardLinearXYZtoRGB = mkPixelRGB . inpmApply (inpmStandard :: INPM cs i)
{-# INLINE standardLinearXYZtoRGB #-}



-- | Derived linear transformation of @RGB@ to @CIE1931 `XYZ`@ colorspace.
derivedLinearRGBtoXYZ ::
     forall cs i e. (DerivedRGB cs i, Elevator e)
  => Pixel (cs i) e
  -> Pixel XYZ Double
derivedLinearRGBtoXYZ = npmApply (npmDerived :: NPM cs i) . unPixelRGB
{-# INLINE derivedLinearRGBtoXYZ #-}

-- | Derived linear transformation of @CIE1931 `XYZ`@ to @RGB@ colorspace.
derivedLinearXYZtoRGB ::
     forall cs i e. (DerivedRGB cs i, Elevator e)
  => Pixel XYZ Double
  -> Pixel (cs i) e
derivedLinearXYZtoRGB = mkPixelRGB . inpmApply (inpmDerived :: INPM cs i)
{-# INLINE derivedLinearXYZtoRGB #-}



-- | Get the `WhitePoint` of chromaticity. `Chromaticity` itself isn't actually evaluated,
-- its type carries enough information for this operation.
--
-- @since 0.1.0
chromaticityWhitePoint :: DerivedRGB cs i => Chromaticity cs i -> WhitePoint i
chromaticityWhitePoint _ = whitePoint
{-# INLINE chromaticityWhitePoint #-}

-- | Get the `Chromaticity` of a pixel in RGB color space. Pixel itself isn't actually
-- evaluated, its type carries enough information for this operation.
--
-- @since 0.1.0
pixelChromaticity :: DerivedRGB cs i => Pixel (cs i) e -> Chromaticity cs i
pixelChromaticity _ = chromaticity
{-# INLINE pixelChromaticity #-}


--derivedLinearRGBtoXYZ

--derivedRGBtoXYZ = npmApply npmStandard . fmap itransfer . coerce

--standardRGBtoXYZ
