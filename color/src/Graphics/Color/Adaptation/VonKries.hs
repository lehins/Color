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
{-# LANGUAGE TypeInType #-}
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
  , ChromaticAdaptationTransform
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
import Data.Typeable

data VonKries
  = VonKries
  -- ^ VonKries chromatic adaptation transform matrix
  --
  -- >>> cat :: CAT 'VonKries Float
  -- CAT VonKries 'VonKries Float
  -- [ [ 0.400240, 0.707600,-0.080810 ]
  -- , [-0.226300, 1.165320, 0.045700 ]
  -- , [ 0.000000, 0.000000, 0.918220 ] ]
  -- >>> icat :: ICAT 'VonKries Float
  -- ICAT VonKries 'VonKries Float
  -- [ [ 1.859936,-1.129382, 0.219897 ]
  -- , [ 0.361191, 0.638812,-0.000006 ]
  -- , [-0.000000,-0.000000, 1.089064 ] ]
  | Bradford
  -- ^ Bradford chromatic adaptation transform matrix
  --
  -- >>> cat :: CAT 'Bradford Float
  -- CAT VonKries 'Bradford Float
  -- [ [ 0.895100, 0.266400,-0.161400 ]
  -- , [-0.750200, 1.713500, 0.036700 ]
  -- , [ 0.038900,-0.068500, 1.029600 ] ]
  -- >>> icat :: ICAT 'Bradford Float
  -- ICAT VonKries 'Bradford Float
  -- [ [ 0.986993,-0.147054, 0.159963 ]
  -- , [ 0.432305, 0.518360, 0.049291 ]
  -- , [-0.008529, 0.040043, 0.968487 ] ]
  | Fairchild
  -- ^ Fairchild chromatic adaptation transform matrix
  --
  -- >>> cat :: CAT 'Fairchild Float
  -- CAT VonKries 'Fairchild Float
  -- [ [ 0.856200, 0.337200,-0.193400 ]
  -- , [-0.836000, 1.832700, 0.003300 ]
  -- , [ 0.035700,-0.046900, 1.011200 ] ]
  -- >>> icat :: ICAT 'Fairchild Float
  -- ICAT VonKries 'Fairchild Float
  -- [ [ 0.987400,-0.176825, 0.189425 ]
  -- , [ 0.450435, 0.464933, 0.084632 ]
  -- , [-0.013968, 0.027807, 0.986162 ] ]
  | CIECAM02
  -- ^ CIECAM02 chromatic adaptation transform matrix
  --
  -- >>> cat :: CAT 'CIECAM02 Float
  -- CAT VonKries 'CIECAM02 Float
  -- [ [ 0.732800, 0.429600,-0.162400 ]
  -- , [-0.703600, 1.697500, 0.006100 ]
  -- , [ 0.003000, 0.013600, 0.983400 ] ]
  -- >>> icat :: ICAT 'CIECAM02 Float
  -- ICAT VonKries 'CIECAM02 Float
  -- [ [ 1.096124,-0.278869, 0.182745 ]
  -- , [ 0.454369, 0.473533, 0.072098 ]
  -- , [-0.009628,-0.005698, 1.015326 ] ]

-- | Chromatic adaptation transformation matrix matrix
newtype CAT (t :: k) e =
  CAT (M3x3 e)
  deriving (Eq)

instance (Typeable t, Typeable k, Elevator e) => Show (CAT (t :: k) e) where
  show m@(CAT m3x3) = asProxy m showsType "\n" ++ show m3x3

-- | Inverse of chromatic adaptation transformation matrix
newtype ICAT (t :: k) e =
  ICAT (M3x3 e)
  deriving (Eq)

instance (Typeable t, Typeable k, Elevator e) => Show (ICAT (t :: k) e) where
  show m@(ICAT m3x3) = asProxy m showsType "\n" ++ show m3x3

icat :: forall t e . (ChromaticAdaptationTransform t, RealFloat e) => ICAT t e
icat = ICAT (invertM3x3 m3x3)
  where CAT m3x3 = cat :: CAT t e

class ChromaticAdaptationTransform (t :: VonKries) where
  cat :: RealFloat e => CAT t e

instance ChromaticAdaptationTransform 'VonKries where
  cat = CAT (M3x3 (V3  0.40024 0.70760 -0.08081)
                  (V3 -0.22630 1.16532  0.04570)
                  (V3  0.00000 0.00000  0.91822))

instance ChromaticAdaptationTransform 'Bradford where
  cat = CAT (M3x3 (V3  0.8951  0.2664 -0.1614)
                  (V3 -0.7502  1.7135  0.0367)
                  (V3  0.0389 -0.0685  1.0296))

instance ChromaticAdaptationTransform 'Fairchild where
  cat = CAT (M3x3 (V3  0.8562  0.3372 -0.1934)
                  (V3 -0.8360  1.8327  0.0033)
                  (V3  0.0357 -0.0469  1.0112))


instance ChromaticAdaptationTransform 'CIECAM02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.0030  0.0136  0.9834))

instance (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
         ChromaticAdaptation (t :: VonKries) (it :: kt) (ir :: kr) e where
  newtype Adaptation (t :: VonKries) (it :: kt) (ir :: kr) e =
    AdaptationMatrix (M3x3 e) deriving (Eq)
  adaptColorXYZ (AdaptationMatrix m3x3) px = coerce (multM3x3byV3 m3x3 (coerce px))
  {-# INLINE adaptColorXYZ #-}

-- | Helper show type for the poly kinded illuminant
data I (i :: k) = I deriving Show

instance (Illuminant it, Illuminant ir, Elevator e) =>
         Show (Adaptation (t :: VonKries) (it :: kt) (ir :: kr) e) where
  showsPrec _ (AdaptationMatrix m3x3) =
    ("AdaptationMatrix (" ++) .
    showsType (Proxy :: Proxy (I (it :: kt))) .
    (") (" ++) . showsType (Proxy :: Proxy (I (ir :: kr))) . (")\n" ++) . shows m3x3

adaptationMatrix ::
     forall t it ir e. (ChromaticAdaptationTransform t, ChromaticAdaptation t it ir e)
  => Adaptation (t :: VonKries) it ir e
adaptationMatrix =
  AdaptationMatrix (multM3x3byM3x3 (multM3x3byV3d im3x3 diag) m3x3)
  where
    diag = multM3x3byV3 m3x3 wpRef / multM3x3byV3 m3x3 wpTest
    CAT m3x3 = cat :: CAT t e
    ICAT im3x3 = icat :: ICAT t e
    wpTest = coerce (whitePointTristimulus :: Color (XYZ it) e)
    wpRef = coerce (whitePointTristimulus :: Color (XYZ ir) e)
{-# NOINLINE adaptationMatrix #-}

vonKriesAdaptation :: ChromaticAdaptation 'VonKries it ir e => Adaptation 'VonKries it ir e
vonKriesAdaptation = adaptationMatrix
{-# INLINE vonKriesAdaptation #-}

fairchildAdaptation :: ChromaticAdaptation 'Fairchild it ir e => Adaptation 'Fairchild it ir e
fairchildAdaptation = adaptationMatrix
{-# INLINE fairchildAdaptation #-}

bradfordAdaptation :: ChromaticAdaptation 'Bradford it ir e => Adaptation 'Bradford it ir e
bradfordAdaptation = adaptationMatrix
{-# INLINE bradfordAdaptation #-}

ciecam02Adaptation :: ChromaticAdaptation 'CIECAM02 it ir e => Adaptation 'CIECAM02 it ir e
ciecam02Adaptation = adaptationMatrix
{-# INLINE ciecam02Adaptation #-}

-- | This function allows conversion of a color between any two color spaces. It uses a
-- very common `VonKries` chromatic adaptation transform with `Bradford` matrix. One of
-- more general functions `Graphics.Color.Adaptation.convertWith` or
-- `Graphics.Color.Adaptation.convertElevatedWith` can be used for selecting another
-- chromatic adaptation algorithm.
--
-- @since 0.1.0
convert :: (ColorSpace cs' i' e', ColorSpace cs i e) => Color cs' e' -> Color cs e
convert = convertElevatedWith (adaptationMatrix @'Bradford @_ @_ @Double)
{-# INLINE convert #-}


-- RAL adopted: Daffodil yellow
-- toWord8 <$> (fromColorXYZ (chromaticAdaptationXYZ (vonKriesAdaptation :: VonKriesAdaptation Bradford D50a D65 Float) (toColorXYZ (ColorLAB 66.5 27.308 80.402 :: Color (LAB D50a) Float) :: Color XYZ Float)) :: Color SRGB Float)
-- <RGB:(226,141,  0)>


--
-- toWord8 <$> (fromColorXYZ (chromaticAdaptationXYZ (vonKriesAdaptation :: VonKriesAdaptation Bradford D50a D65 Double) (toColorXYZ (ColorLAB 83.353 3.462 75.829 :: Color (LAB D50a) Double) :: Color XYZ Double)) :: Color SRGB Double)
-- <RGB:(242,203, 46)>


-- -- Green beige
-- toWord8 <$> (fromColorXYZ (chromaticAdaptationXYZ (vonKriesAdaptation :: VonKriesAdaptation Bradford D50a D65 Double) (toColorXYZ (ColorLAB 76.022 (-0.366) 27.636 :: Color (LAB D50a) Double) :: Color XYZ Double)) :: Color SRGB Double)
-- <RGB:(201,187,136)>

-- From srgb spec
-- srgbVonKries :: VonKriesAdaptationMatrix t it ir Double
-- srgbVonKries = VonKriesAdaptationMatrix $ M3x3
--       (V3  1.047844353856414 0.022898981050086 -0.050206647741605)
--       (V3  0.029549007606644 0.990508028941971 -0.017074711360960)
--       (V3 -0.009250984365223 0.015072338237051  0.751717835079977)
