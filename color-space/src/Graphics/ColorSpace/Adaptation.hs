{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.ColorSpace.CIE1931
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Adaptation
  ( VonKriesTransform(..)
  , VonKriesAdaptationMatrix(..)
  , vonKriesAdaptationMatrix
  , CAT(..)
  , ICAT(..)
  , icat
  ) where

import Data.Coerce
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.CIE1931.Illuminant
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal
import Graphics.ColorSpace.RGB.SRGB


-- | Chromatic adaptation transformation matrix
newtype CAT t e = CAT (M3x3 e)

-- | Inverse of chromatic adaptation transformation matrix
newtype ICAT t e = ICAT (M3x3 e)

class VonKriesTransform t where

  cat :: RealFloat e => CAT t e

icat :: forall t e . (VonKriesTransform t, RealFloat e) => ICAT t e
icat = ICAT (invertM3x3 m3x3)
  where CAT m3x3 = cat :: CAT t e

data CIECAM02

instance VonKriesTransform CIECAM02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.003   0.0136  0.9834))

data Bradford

instance VonKriesTransform Bradford where
  cat = CAT (M3x3 (V3  0.8951  0.2664 -0.1614)
                  (V3 -0.7502  1.7135  0.0367)
                  (V3  0.0389 -0.0685  1.0296))

data D50a

instance Illuminant D50a where
  type Temperature D50a = 5003
  whitePoint = WhitePoint 0.3457 0.3585


newtype VonKriesAdaptationMatrix t (it :: tk) (ir :: rk) e =
  VonKriesAdaptationMatrix (M3x3 e)
  deriving Eq

instance Elevator e => Show (VonKriesAdaptationMatrix t it ir e) where
  show (VonKriesAdaptationMatrix m3x3) = "VonKriesAdaptationMatrix:\n" ++ show m3x3


vonKriesAdaptationMatrix ::
  forall t it ir e . (VonKriesTransform t, Illuminant it, Illuminant ir, Elevator e, RealFloat e)
  => VonKriesAdaptationMatrix t it ir e
vonKriesAdaptationMatrix =
  VonKriesAdaptationMatrix (multM3x3byM3x3 (multM3x3byV3d im3x3 diag) m3x3)
  where
    diag = multM3x3byV3 m3x3 wpRef / multM3x3byV3 m3x3 wpTest
    CAT m3x3 = cat :: CAT t e
    ICAT im3x3 = icat :: ICAT t e
    wpTest = coerce (normalTristimulus :: Tristimulus it e)
    wpRef = coerce (normalTristimulus :: Tristimulus ir e)


-- chromaticAdaptation ::
--      forall ir cs it e t. (Illuminant ir, Illuminant it, ColorSpace cs e, RealFloat e)
--   => Chromaticity cs it e
--   -> CAT t e
--   -> Chromaticity cs ir e
-- chromaticAdaptation c cat = Chromaticity redPrimary greenPrimary bluePrimary
--   where
--     wpTest = normalTristimulus :: Tristimulus i e
--     wpReference = normalTristimulus :: Tristimulus i' e
--     redPrimary = vonKriesAdaptation (chromaRed c) wpTest wpReference cat
--     greenPrimary = vonKriesAdaptation (chromaGreen c) wpTest wpReference cat
--     bluePrimary = vonKriesAdaptation (chromaBlue c) wpTest wpReference cat
--   -- Primary $ fromPixelXYZ $ PixelXYZ $ mutM3x3 m3x3 $ coerce $ primaryXYZ primary

-- class ColorAppearanceModel cam where
--   adoptationMatrix :: CAM t
