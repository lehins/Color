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
  ( -- * Color conversion
    convert
    -- * Von Kries adaptation
  , VonKries(..)
  , CAT(..)
  , ICAT(..)
  , ChromaticAdaptationTransform
  , cat
  , icat
  , vonKriesAdaptation
  , bradfordAdaptation
  , fairchildAdaptation
  , ciecat02Adaptation
  , cmccat2000Adaptation
  , adaptationMatrix
  -- * Deprecated
  , CIECAM02
  , ciecam02Adaptation
  ) where

import Data.Coerce
import Data.Proxy
import Graphics.Color.Adaptation.Internal
import Graphics.Color.Algebra
import Graphics.Color.Space.Internal
import Data.Typeable


data VonKries
  = VonKries
  -- ^ VonKries chromatic adaptation transform
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
  --
  -- @since 0.1.0
  | Bradford
  -- ^ Bradford chromatic adaptation transform
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
  --
  -- @since 0.1.0
  | Fairchild
  -- ^ Fairchild chromatic adaptation transform
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
  --
  -- @since 0.1.0
  | CIECAT02
  -- ^ CIECAT02 chromatic adaptation transform
  --
  -- >>> cat :: CAT 'CIECAT02 Float
  -- CAT VonKries 'CIECAT02 Float
  -- [ [ 0.732800, 0.429600,-0.162400 ]
  -- , [-0.703600, 1.697500, 0.006100 ]
  -- , [ 0.003000, 0.013600, 0.983400 ] ]
  -- >>> icat :: ICAT 'CIECAT02 Float
  -- ICAT VonKries 'CIECAT02 Float
  -- [ [ 1.096124,-0.278869, 0.182745 ]
  -- , [ 0.454369, 0.473533, 0.072098 ]
  -- , [-0.009628,-0.005698, 1.015326 ] ]
  --
  -- @since 0.1.3
  | CMCCAT2000
  -- ^ CMCCAT2000 chromatic adaptation transform
  --
  -- >>> cat :: CAT 'CMCCAT2000 Float
  -- CAT VonKries 'CMCCAT2000 Float
  -- [ [ 0.798200, 0.338900,-0.137100 ]
  -- , [-0.591800, 1.551200, 0.040600 ]
  -- , [ 0.000800, 0.023900, 0.975300 ] ]
  -- >>> icat :: ICAT 'CMCCAT2000 Float
  -- ICAT VonKries 'CMCCAT2000 Float
  -- [ [ 1.076450,-0.237662, 0.161212 ]
  -- , [ 0.410964, 0.554342, 0.034694 ]
  -- , [-0.010954,-0.013389, 1.024343 ] ]
  --
  -- @since 0.1.3

-- | Chromatic adaptation transformation matrix
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

instance ChromaticAdaptationTransform 'CIECAT02 where
  cat = CAT (M3x3 (V3  0.7328  0.4296 -0.1624)
                  (V3 -0.7036  1.6975  0.0061)
                  (V3  0.0030  0.0136  0.9834))

instance ChromaticAdaptationTransform 'CMCCAT2000 where
  cat = CAT (M3x3 (V3  0.7982  0.3389 -0.1371)
                  (V3 -0.5918  1.5512  0.0406)
                  (V3  0.0008  0.0239  0.9753))

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

-- | `VonKries` chromatic adaptation transform.
--
-- @since 0.1.2
vonKriesAdaptation :: ChromaticAdaptation 'VonKries it ir e => Adaptation 'VonKries it ir e
vonKriesAdaptation = adaptationMatrix
{-# INLINE vonKriesAdaptation #-}

-- | `Fairchild` chromatic adaptation transform.
--
-- @since 0.1.2
fairchildAdaptation :: ChromaticAdaptation 'Fairchild it ir e => Adaptation 'Fairchild it ir e
fairchildAdaptation = adaptationMatrix
{-# INLINE fairchildAdaptation #-}

-- | `Bradford` chromatic adaptation transform, as defined in the
-- [CIECAM97s](https://en.wikipedia.org/wiki/Color_appearance_model#CIECAM97s) color
-- appearance model.
--
-- @since 0.1.2
bradfordAdaptation :: ChromaticAdaptation 'Bradford it ir e => Adaptation 'Bradford it ir e
bradfordAdaptation = adaptationMatrix
{-# INLINE bradfordAdaptation #-}

-- | `CIECAT02` chromatic adaptation as defined it
-- [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02) color appearance model
--
-- @since 0.1.3
ciecat02Adaptation :: ChromaticAdaptation 'CIECAT02 it ir e => Adaptation 'CIECAT02 it ir e
ciecat02Adaptation = adaptationMatrix
{-# INLINE ciecat02Adaptation #-}

-- | `CMCCAT2000` chromatic adaptation. Predecessor of `CIECAT02`. as defined it
-- [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02) color appearance model
--
-- @since 0.1.3
cmccat2000Adaptation :: ChromaticAdaptation 'CMCCAT2000 it ir e => Adaptation 'CIECAT02 it ir e
cmccat2000Adaptation = adaptationMatrix
{-# INLINE cmccat2000Adaptation #-}

type CIECAM02 = 'CIECAT02
{-# DEPRECATED CIECAM02 "In favor of a proper name 'CIECAT02'" #-}

ciecam02Adaptation :: ChromaticAdaptation CIECAM02 it ir e => Adaptation CIECAM02 it ir e
ciecam02Adaptation = adaptationMatrix
{-# INLINE ciecam02Adaptation #-}
{-# DEPRECATED ciecam02Adaptation "In favor of a proper name 'ciecat02Adaptation'" #-}

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
