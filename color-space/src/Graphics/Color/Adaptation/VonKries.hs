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
-- Module      : Graphics.Color.Adaptation.VonKries
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Adaptation.VonKries
  ( VonKries(..)
  , convert
  , CAT(..)
  , ICAT(..)
  , ColorAdaptationTransform
  , cat
  , icat
  , vonKriesAdaptation
  , bradfordAdaptation
  , fairchildAdaptation
  , ciecam02Adaptation
  , adaptationMatrix
  ) where

import Data.Coerce
import Data.Proxy
import Graphics.Color.Adaptation.Internal
import Graphics.Color.Algebra
import Graphics.Color.Space.Internal
--import Graphics.Color.Illuminant.CIE1931
--import Graphics.Color.Space.RGB.SRGB

data VonKries
  = VonKries
  | Bradford
  | Fairchild
  | CIECAM02

-- | Chromatic adaptation transformation matrix
newtype CAT t e =
  CAT (M3x3 e)
  deriving (Eq, Show)

-- | Inverse of chromatic adaptation transformation matrix
newtype ICAT t e =
  ICAT (M3x3 e)
  deriving (Eq, Show)

icat :: forall t e . (ColorAdaptationTransform t, RealFloat e) => ICAT t e
icat = ICAT (invertM3x3 m3x3)
  where CAT m3x3 = cat :: CAT t e

class ColorAdaptationTransform (t :: VonKries) where
  cat :: RealFloat e => CAT t e

instance ColorAdaptationTransform 'VonKries where
  cat = CAT (M3x3 (V3  0.40024 0.70760 -0.08081)
                  (V3 -0.22630 1.16532  0.04570)
                  (V3  0.00000 0.00000  0.91822))

instance ColorAdaptationTransform 'Bradford where
  cat = CAT (M3x3 (V3  0.8951  0.2664 -0.1614)
                  (V3 -0.7502  1.7135  0.0367)
                  (V3  0.0389 -0.0685  1.0296))

instance ColorAdaptationTransform 'Fairchild where
  cat = CAT (M3x3 (V3  0.8562  0.3372 -0.1934)
                  (V3 -0.8360  1.8327  0.0033)
                  (V3  0.0357 -0.0469  1.0112))


instance ColorAdaptationTransform 'CIECAM02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.0030  0.0136  0.9834))

instance (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
         ColorAdaptation (t :: VonKries) (it :: kt) (ir :: kr) e where
  newtype Adaptation (t :: VonKries) (it :: kt) (ir :: kr) e =
    AdaptationMatrix (M3x3 e) deriving (Eq)
  adaptPixelXYZ (AdaptationMatrix m3x3) px = coerce (multM3x3byV3 m3x3 (coerce px))

-- | Helper show type for the poly kinded illuminant
data I (i :: k) = I deriving Show

instance (Illuminant it, Illuminant ir, Elevator e) =>
         Show (Adaptation (t :: VonKries) (it :: kt) (ir :: kr) e) where
  showsPrec _ (AdaptationMatrix m3x3) =
    ("AdaptationMatrix (" ++) .
    showsType (Proxy :: Proxy (I (it :: kt))) .
    (") (" ++) . showsType (Proxy :: Proxy (I (ir :: kr))) . (")\n" ++) . shows m3x3

adaptationMatrix ::
     forall t it ir e. (ColorAdaptationTransform t, ColorAdaptation t it ir e)
  => Adaptation (t :: VonKries) it ir e
adaptationMatrix =
  AdaptationMatrix (multM3x3byM3x3 (multM3x3byV3d im3x3 diag) m3x3)
  where
    diag = multM3x3byV3 m3x3 wpRef / multM3x3byV3 m3x3 wpTest
    CAT m3x3 = cat :: CAT t e
    ICAT im3x3 = icat :: ICAT t e
    wpTest = coerce (normalTristimulus :: Tristimulus it e)
    wpRef = coerce (normalTristimulus :: Tristimulus ir e)

vonKriesAdaptation :: ColorAdaptation 'VonKries it ir e => Adaptation 'VonKries it ir e
vonKriesAdaptation = adaptationMatrix

fairchildAdaptation :: ColorAdaptation 'Fairchild it ir e => Adaptation 'Fairchild it ir e
fairchildAdaptation = adaptationMatrix

bradfordAdaptation :: ColorAdaptation 'Bradford it ir e => Adaptation 'Bradford it ir e
bradfordAdaptation = adaptationMatrix

ciecam02Adaptation :: ColorAdaptation 'CIECAM02 it ir e => Adaptation 'CIECAM02 it ir e
ciecam02Adaptation = adaptationMatrix


convert :: (ColorSpace cs2 i2 e2, ColorSpace cs1 i1 e1) => Pixel cs2 e2 -> Pixel cs1 e1
convert = convertColorSpace (adaptationMatrix @'Bradford @_ @_ @Double)



-- RAL adopted: Daffodil yellow
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Float) (toPixelXYZ (PixelLAB 66.5 27.308 80.402 :: Pixel (LAB D50a) Float) :: Pixel XYZ Float)) :: Pixel SRGB Float)
-- <RGB:(226,141,  0)>


--
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 83.353 3.462 75.829 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
-- <RGB:(242,203, 46)>


-- -- Green beige
-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 76.022 (-0.366) 27.636 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
-- <RGB:(201,187,136)>

data D50

instance Illuminant D50 where
  type Temperature D50 = 5003
  whitePoint = WhitePoint 0.34567 0.35850

data D65

instance Illuminant D65 where
  type Temperature D65 = 6504
  whitePoint = WhitePoint 0.31271 0.32902


-- srgbVonKries :: VonKriesAdaptationMatrix t it ir Double
-- srgbVonKries = VonKriesAdaptationMatrix $ M3x3
--       (V3 1.047844353856414 0.022898981050086 0.050206647741605)
--       (V3 0.029549007606644 0.990508028941971 0.017074711360960)
--       (V3 0.009250984365223 0.015072338237051 0.751717835079977)
