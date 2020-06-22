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
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
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
  -- [ [ 0.40024000, 0.70760000,-0.08081000 ]
  -- , [-0.22630000, 1.16532000, 0.04570000 ]
  -- , [ 0.00000000, 0.00000000, 0.91822000 ] ]
  -- >>> icat :: ICAT 'VonKries Float
  -- ICAT VonKries 'VonKries Float
  -- [ [ 1.85993650,-1.12938170, 0.21989742 ]
  -- , [ 0.36119142, 0.63881250,-0.00000637 ]
  -- , [-0.00000000,-0.00000000, 1.08906360 ] ]
  --
  -- @since 0.1.0
  | Bradford
  -- ^ Bradford chromatic adaptation transform
  --
  -- >>> cat :: CAT 'Bradford Float
  -- CAT VonKries 'Bradford Float
  -- [ [ 0.89510000, 0.26640000,-0.16140000 ]
  -- , [-0.75020000, 1.71350000, 0.03670000 ]
  -- , [ 0.03890000,-0.06850000, 1.02960000 ] ]
  -- >>> icat :: ICAT 'Bradford Float
  -- ICAT VonKries 'Bradford Float
  -- [ [ 0.98699290,-0.14705427, 0.15996265 ]
  -- , [ 0.43230528, 0.51836026, 0.04929122 ]
  -- , [-0.00852867, 0.04004282, 0.96848667 ] ]
  --
  -- @since 0.1.0
  | Fairchild
  -- ^ Fairchild chromatic adaptation transform
  --
  -- >>> cat :: CAT 'Fairchild Float
  -- CAT VonKries 'Fairchild Float
  -- [ [ 0.85620000, 0.33720000,-0.19340000 ]
  -- , [-0.83600000, 1.83270000, 0.00330000 ]
  -- , [ 0.03570000,-0.04690000, 1.01120000 ] ]
  -- >>> icat :: ICAT 'Fairchild Float
  -- ICAT VonKries 'Fairchild Float
  -- [ [ 0.98739994,-0.17682500, 0.18942511 ]
  -- , [ 0.45043513, 0.46493286, 0.08463200 ]
  -- , [-0.01396833, 0.02780657, 0.98616177 ] ]
  --
  -- @since 0.1.0
  | CIECAT02
  -- ^ CIECAT02 chromatic adaptation transform
  --
  -- >>> cat :: CAT 'CIECAT02 Float
  -- CAT VonKries 'CIECAT02 Float
  -- [ [ 0.73280000, 0.42960000,-0.16240000 ]
  -- , [-0.70360000, 1.69750000, 0.00610000 ]
  -- , [ 0.00300000, 0.01360000, 0.98340000 ] ]
  -- >>> icat :: ICAT 'CIECAT02 Float
  -- ICAT VonKries 'CIECAT02 Float
  -- [ [ 1.09612380,-0.27886900, 0.18274519 ]
  -- , [ 0.45436904, 0.47353318, 0.07209781 ]
  -- , [-0.00962761,-0.00569803, 1.01532570 ] ]
  --
  -- @since 0.1.3
  | CMCCAT2000
  -- ^ CMCCAT2000 chromatic adaptation transform
  --
  -- >>> cat :: CAT 'CMCCAT2000 Float
  -- CAT VonKries 'CMCCAT2000 Float
  -- [ [ 0.79820000, 0.33890000,-0.13710000 ]
  -- , [-0.59180000, 1.55120000, 0.04060000 ]
  -- , [ 0.00080000, 0.02390000, 0.97530000 ] ]
  -- >>> icat :: ICAT 'CMCCAT2000 Float
  -- ICAT VonKries 'CMCCAT2000 Float
  -- [ [ 1.07645010,-0.23766239, 0.16121234 ]
  -- , [ 0.41096430, 0.55434180, 0.03469386 ]
  -- , [-0.01095376,-0.01338936, 1.02434310 ] ]
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
