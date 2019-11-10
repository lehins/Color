{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
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
  , pattern PixelRGB
  , pattern PixelRGBA
  , Chromaticity(..)
  , rgb2xyz
  , xyz2rgb
  , NPM(..)
  , npmDerive
  , INPM(..)
  , inpmDerive
  , pixelChromaticity
  , pixelWhitePoint
  , chromaticityWhitePoint
  ) where

import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Data.Coerce

class Illuminant i => RedGreenBlue cs (i :: k) | cs -> i where
  -- | RGB primaries that are defined for the RGB color space, while point is defined by
  -- the __@i@__ type parameter
  chromaticity :: Chromaticity cs i

  -- | Encoding color component transfer function (inverse). Also known as opto-electronic
  -- transfer function (OETF / OECF) or gamma function.
  ecctf :: (RealFloat a, Elevator a) => Pixel cs a -> Pixel cs a

  -- | Decoding color component transfer function (forward)
  dcctf :: (RealFloat a, Elevator a) => Pixel cs a -> Pixel cs a

  -- | Normalized primary matrix for this RGB color space. Default implementation derives
  -- it from `chromaticity`
  npm :: (Elevator a, RealFloat a) => NPM cs i a
  npm = npmDerive chromaticity

  -- | Inverse normalized primary matrix for this RGB color space. Default implementation
  -- derives it from `chromaticity`
  inpm :: (Elevator a, RealFloat a) => INPM cs i a
  inpm = inpmDerive chromaticity

  -- | Linear transformation of a pixel in a linear RGB color space into XYZ color space
  npmApply :: forall a . (Elevator a, RealFloat a) => Pixel cs a -> Pixel XYZ a
  npmApply px = coerce (multM3x3byV3 (unNPM (npm :: NPM cs i a)) (V3 r g b))
    where CM.PixelRGB r g b = unPixelRGB px
  {-# INLINE npmApply #-}

  -- | Linear transformation of a pixel in XYZ color space into a linear RGB color space
  inpmApply :: forall a . (Elevator a, RealFloat a) => Pixel XYZ a -> Pixel cs a
  inpmApply xyz =
    mkPixelRGB $ fromV3 CM.PixelRGB (multM3x3byV3 (unINPM (inpm :: INPM cs i a)) (coerce xyz))
  {-# INLINE inpmApply #-}

  -- | Lift RGB color model into a RGB color space
  mkPixelRGB :: Pixel CM.RGB e -> Pixel cs e
  default mkPixelRGB ::
    Coercible (Pixel CM.RGB e) (Pixel cs e) => Pixel CM.RGB e -> Pixel cs e
  mkPixelRGB = coerce

  -- | Drop RGB color space into the RGB color model
  unPixelRGB :: Pixel cs e -> Pixel CM.RGB e
  default unPixelRGB ::
    Coercible (Pixel cs e) (Pixel CM.RGB e) => Pixel cs e -> Pixel CM.RGB e
  unPixelRGB = coerce


data Chromaticity cs i where
  Chromaticity :: Illuminant i =>
    { chromaRed   :: {-# UNPACK #-}!Primary
    , chromaGreen :: {-# UNPACK #-}!Primary
    , chromaBlue  :: {-# UNPACK #-}!Primary
    } -> Chromaticity cs i
deriving instance Eq (Chromaticity cs i)
deriving instance Show (Chromaticity cs i)


rgb2xyz :: (RedGreenBlue cs i, Elevator e, RealFloat e) => Pixel cs e -> Pixel XYZ e
rgb2xyz = npmApply . dcctf
{-# INLINE rgb2xyz #-}

xyz2rgb :: (RedGreenBlue cs i, Elevator e, RealFloat e) => Pixel XYZ e -> Pixel cs e
xyz2rgb = ecctf . inpmApply
{-# INLINE xyz2rgb #-}


-- | Constructor for an RGB color space.
pattern PixelRGB :: RedGreenBlue cs i => e -> e -> e -> Pixel cs e
pattern PixelRGB r g b <- (unPixelRGB -> CM.PixelRGB r g b) where
        PixelRGB r g b = mkPixelRGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}

-- | Constructor for an RGB color space with Alpha channel
pattern PixelRGBA :: RedGreenBlue cs i => e -> e -> e -> e -> Pixel (Alpha cs) e
pattern PixelRGBA r g b a <- Alpha (unPixelRGB -> CM.PixelRGB r g b) a where
        PixelRGBA r g b a = Alpha (mkPixelRGB (CM.PixelRGB r g b)) a
{-# COMPLETE PixelRGBA #-}


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
newtype NPM cs (i :: k) a = NPM
  { unNPM :: M3x3 a
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator a => Show (NPM cs i a) where
  show = show . unNPM

-- | Inverse normalized primary matrix (iNPM), which is used to tranform linear
-- `Graphics.ColorSpace.CIE1931.XYZ.XYZ` color space into an RGB color space. It is
-- literally a matrix inverse of `NPM`
--
-- @since 0.1.0
newtype INPM cs (i :: k) a = INPM
  { unINPM :: M3x3 a
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator a => Show (INPM cs i a) where
  show = show . unINPM


-- | Derive a `NPM` form chromaticities and a white point
--
-- @since 0.1.0
npmDerive ::
     forall cs i a. (Elevator a, RealFloat a, Illuminant i)
  => Chromaticity cs i
  -> NPM cs i a
npmDerive (Chromaticity r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
    -- transposed matrix with xyz primaries
  where
    !primaries' =
      toRealFloat <$>
      M3x3
        (V3 (xPrimary r) (xPrimary g) (xPrimary b))
        (V3 (yPrimary r) (yPrimary g) (yPrimary b))
        (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff =
      invertM3x3 primaries' `multM3x3byV3`
      fmap fromDouble (coerce (whitePointXYZ (whitePoint :: WhitePoint i)))
{-# INLINE npmDerive #-}

-- | Derive an `INPM` form chromaticities and a white point
--
-- @since 0.1.0
inpmDerive ::
     forall cs i a. (Elevator a, RealFloat a, Illuminant i)
  => Chromaticity cs i
  -> INPM cs i a
inpmDerive = INPM . invertM3x3 . unNPM . npmDerive
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
pixelChromaticity :: RedGreenBlue cs i => Pixel cs e -> Chromaticity cs i
pixelChromaticity _ = chromaticity
{-# INLINE pixelChromaticity #-}


-- | Get the white point of any RGB pixel. Pixel itself isn't evaluated, since its type
-- carries enough information for getting the white point.
--
-- >>> import Graphics.ColorSpace.RGB
-- >>> pixelWhitePoint (PixelRGB8 1 2 3)
-- WhitePoint {xWhitePoint = 0.3127, yWhitePoint = 0.329}
--
-- @since 0.1.0
pixelWhitePoint :: RedGreenBlue cs i => Pixel cs e -> WhitePoint i
pixelWhitePoint _ = whitePoint
{-# INLINE pixelWhitePoint #-}

