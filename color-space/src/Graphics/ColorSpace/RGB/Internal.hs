{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Internal
  ( RedGreenBlue(..)
  , fromRGBtoXYZ
  , fromXYZtoRGB
  , NPM(..)
  , npmCompute
  , npmApply
  , INPM(..)
  , inpmCompute
  , inpmApply
  ) where

import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Prelude hiding (map)
import Data.Coerce



class Illuminant i => RedGreenBlue cs i | cs -> i where
  chromaticity :: Chromaticity cs i

  mkPixelRGB :: Pixel CM.RGB e -> Pixel cs e

  unPixelRGB :: Pixel cs e -> Pixel CM.RGB e

  npm :: NPM cs i
  npm = npmCompute chromaticity

  inpm :: INPM cs i
  inpm = inpmCompute chromaticity


newtype NPM cs (i :: k) = NPM
  { unNPM :: M3x3
  } deriving (Eq)

instance Show (NPM cs i) where
  show = show . unNPM

newtype INPM cs (i :: k) = INPM
  { unINPM :: M3x3
  } deriving (Eq)

instance Show (INPM cs i) where
  show = show . unINPM


npmCompute :: forall cs i . Illuminant i => Chromaticity cs i -> NPM cs i
npmCompute (Chromaticity r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
  where
    -- transposed matrix with xyz primaries
    !primaries' = M3x3 (V3 (xPrimary r) (xPrimary g) (xPrimary b))
                      (V3 (yPrimary r) (yPrimary g) (yPrimary b))
                      (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ (whitePoint :: WhitePoint i)
{-# INLINE npmCompute #-}

inpmCompute :: forall cs i . Illuminant i => Chromaticity cs i -> INPM cs i
inpmCompute = coerce . invertM3x3 . coerce . npmCompute
{-# INLINE inpmCompute #-}


npmApply :: (Elevator e1, Elevator e2) => NPM cs i -> Pixel CM.RGB e2 -> Pixel XYZ e1
npmApply npm' (CM.PixelRGB r g b) = fromV3 PixelXYZ (multM3x3byV3 (unNPM npm') (toV3 r g b))
{-# INLINE npmApply #-}

inpmApply :: (Elevator e1, Elevator e2) => INPM cs i -> Pixel XYZ e2 -> Pixel CM.RGB e1
inpmApply inpm' (PixelXYZ x y z) = fromV3 CM.PixelRGB (multM3x3byV3 (unINPM inpm') (toV3 x y z))
{-# INLINE inpmApply #-}


fromRGBtoXYZ ::
     forall cs i e. (RedGreenBlue cs i, ColorModel cs e)
  => Pixel cs e
  -> Pixel XYZ Double
fromRGBtoXYZ = npmApply (npm :: NPM cs i) . unPixelRGB
{-# INLINE fromRGBtoXYZ #-}

fromXYZtoRGB ::
     forall cs i e. (RedGreenBlue cs i, ColorModel cs e)
  => Pixel XYZ Double
  -> Pixel cs e
fromXYZtoRGB = mkPixelRGB . inpmApply (inpm :: INPM cs i)
{-# INLINE fromXYZtoRGB #-}
