{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorAdaptation.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorAdaptation.Internal
  ( ColorAdaptation(..)
  , convert
  ) where

import Data.Proxy
import Data.Coerce
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal
import Graphics.ColorSpace.RGB.Derived.SRGB
import Data.Kind
import Graphics.ColorSpace.CIE1931.Illuminant
import Graphics.ColorSpace.CIE1976.LAB


data VonKries
  = Bradford
  | CIECAM02

instance ColorAdaptationTransform 'Bradford where
  cat = CAT (M3x3 (V3  0.8951  0.2664 -0.1614)
                  (V3 -0.7502  1.7135  0.0367)
                  (V3  0.0389 -0.0685  1.0296))

instance ColorAdaptationTransform 'CIECAM02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.003   0.0136  0.9834))


class (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
      ColorAdaptation (t :: k) (it :: kt) (ir :: kr) e
  where
  data AdaptationParam t it ir e :: Type
  adaptPixelXYZ :: AdaptationParam t it ir e -> Pixel (XYZ it) e -> Pixel (XYZ ir) e

instance (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
         ColorAdaptation (t :: VonKries) (it :: kt) (ir :: kr) e where
  newtype AdaptationParam (t :: VonKries) (it :: kt) (ir :: kr)
          e = AdaptationMatrix (M3x3 e)
                deriving (Eq, Show)
  adaptPixelXYZ (AdaptationMatrix m3x3) px = coerce (multM3x3byV3 m3x3 (coerce px))


chromaticityAdaptation ::
     ColorAdaptation t it ir e
  => AdaptationParam t it ir e
  -> Chromaticity cst it e
  -> Chromaticity csr ir e
chromaticityAdaptation param c = Chromaticity redPrimary greenPrimary bluePrimary
  where
    applyMatrix chroma = PrimaryChroma (fromPixelXYZ (adaptPixelXYZ param (primaryXYZ chroma)))
    redPrimary = applyMatrix (chromaRed c)
    greenPrimary = applyMatrix (chromaGreen c)
    bluePrimary = applyMatrix (chromaBlue c)

-- convertColorSpace ::
--      (ColorAdaptation t i' i e, ColorSpace cs' i' e, ColorSpace cs i e)
--   => AdaptationParam t i' i e
--   -> Pixel cs' e
--   -> Pixel cs e
convertColorSpace ::
     (ColorAdaptation t i2 i1 a, ColorSpace cs1 i1 e1, ColorSpace cs2 i2 e2)
  => AdaptationParam t i2 i1 a
  -> Pixel cs2 e2
  -> Pixel cs1 e1
convertColorSpace param = fromPixelXYZ . adaptPixelXYZ param . toPixelXYZ

convert :: (ColorSpace cs2 i2 e2, ColorSpace cs1 i1 e1) => Pixel cs2 e2 -> Pixel cs1 e1
convert = convertColorSpace (adaptationMatrix @'Bradford @_ @_ @Double)

-- | Chromatic adaptation transformation matrix
newtype CAT t e =
  CAT (M3x3 e)
  deriving (Eq, Show)

-- | Inverse of chromatic adaptation transformation matrix
newtype ICAT t e =
  ICAT (M3x3 e)
  deriving (Eq, Show)

class ColorAdaptationTransform (t :: VonKries) where
  cat :: RealFloat e => CAT t e


icat :: forall t e . (ColorAdaptationTransform t, RealFloat e) => ICAT t e
icat = ICAT (invertM3x3 m3x3)
  where CAT m3x3 = cat :: CAT t e


adaptationMatrix ::
     forall t it ir e. (ColorAdaptationTransform t, ColorAdaptation t it ir e)
  => AdaptationParam (t :: VonKries) it ir e
adaptationMatrix =
  AdaptationMatrix (multM3x3byM3x3 (multM3x3byV3d im3x3 diag) m3x3)
  where
    diag = multM3x3byV3 m3x3 wpRef / multM3x3byV3 m3x3 wpTest
    CAT m3x3 = cat :: CAT t e
    ICAT im3x3 = icat :: ICAT t e
    wpTest = coerce (normalTristimulus :: Tristimulus it e)
    wpRef = coerce (normalTristimulus :: Tristimulus ir e)

bradfordAdaptation :: ColorAdaptation 'Bradford it ir e => AdaptationParam 'Bradford it ir e
bradfordAdaptation = adaptationMatrix

ciecam02Adaptation :: ColorAdaptation 'CIECAM02 it ir e => AdaptationParam 'CIECAM02 it ir e
ciecam02Adaptation = adaptationMatrix


-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 76.022 (-0.366) 27.636 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
