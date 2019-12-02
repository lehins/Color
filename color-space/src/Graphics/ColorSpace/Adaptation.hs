{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
import Graphics.ColorSpace.CIE1976.LAB
import Graphics.ColorSpace.Internal
-- import Graphics.ColorSpace.RGB.Internal
-- import Graphics.ColorSpace.RGB.Derived.AdobeRGB as Der
-- import Graphics.ColorSpace.RGB.AdobeRGB as Std


-- | Chromatic adaptation transformation matrix
newtype CAT t e = CAT (M3x3 e)

-- | Inverse of chromatic adaptation transformation matrix
newtype ICAT t e = ICAT (M3x3 e)

class VonKriesTransform t where

  cat :: RealFloat e => CAT t e

icat :: forall t e . (VonKriesTransform t, RealFloat e) => ICAT t e
icat = ICAT (invertM3x3 m3x3)
  where CAT m3x3 = cat :: CAT t e

data CIECAM02 = CIECAM02

instance VonKriesTransform CIECAM02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.003   0.0136  0.9834))

data Bradford = Bradford

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


-- chromaticityAdaptation ::
--      forall ir csr it cs e t.
--      (VonKriesTransform t, Illuminant it, Illuminant ir, ColorSpace cs e, ColorSpace csr e, RealFloat e)
--   => t
--   -> Chromaticity cs it e
--   -> Chromaticity csr ir e
-- chromaticityAdaptation _ c = Chromaticity redPrimary greenPrimary bluePrimary
--   where
--     VonKriesAdaptationMatrix m3x3 = vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix t it ir e
--     applyMatrix chroma =
--       PrimaryChroma (fromPixelXYZ (XYZ (multM3x3byV3 m3x3 (coerce (primaryXYZ chroma)))))
--     redPrimary = applyMatrix (chromaRed c)
--     greenPrimary = applyMatrix (chromaGreen c)
--     bluePrimary = applyMatrix (chromaBlue c)



chromaticAdaptationXYZ ::
     forall ir it e t. (VonKriesTransform t, Illuminant it, Illuminant ir, RealFloat e)
  => VonKriesAdaptationMatrix t it ir e
  -> Pixel (XYZ it) e
  -> Pixel (XYZ ir) e
chromaticAdaptationXYZ (VonKriesAdaptationMatrix m3x3) px = coerce (multM3x3byV3 m3x3 (coerce px))



chromaticAdaptation ::
     forall csr ir cst it e t.
     ( VonKriesTransform t
     , Illuminant it
     , Illuminant ir
     , ColorSpace cst it e
     , ColorSpace csr ir e
     , RealFloat e
     )
  => t
  -> Pixel cst e
  -> Pixel csr e
chromaticAdaptation _ px =
  fromPixelXYZ (XYZ (multM3x3byV3 m3x3 (coerce (toPixelXYZ px :: Pixel (XYZ it) e))))
  where
    VonKriesAdaptationMatrix m3x3 = vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix t it ir e

srgbVonKries :: VonKriesAdaptationMatrix t it ir Double
srgbVonKries = VonKriesAdaptationMatrix $ M3x3
      (V3 1.047844353856414 0.022898981050086 0.050206647741605)
      (V3 0.029549007606644 0.990508028941971 0.017074711360960)
      (V3 0.009250984365223 0.015072338237051 0.751717835079977)

-- RAL adopted: Daffodil yellow
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Float) (toPixelXYZ (PixelLAB 66.5 27.308 80.402 :: Pixel (LAB D50a) Float) :: Pixel XYZ Float)) :: Pixel SRGB Float)
-- <RGB:(226,141,  0)>


--
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 83.353 3.462 75.829 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
-- <RGB:(242,203, 46)>


-- -- Green beige
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 76.022 (-0.366) 27.636 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
-- <RGB:(201,187,136)>
