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
  , rgbLuminocity
  , NPM(..)
  , npmApply
  , npmDerive
  , INPM(..)
  , inpmApply
  , inpmDerive
  , pixelChromaticity
  , pixelWhitePoint
  , chromaticityWhitePoint
  , module Graphics.ColorSpace.Algebra
  ) where

import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Y
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Data.Coerce


class Illuminant i => RedGreenBlue cs (i :: k) | cs -> i where
  -- | RGB primaries that are defined for the RGB color space, while point is defined by
  -- the __@i@__ type parameter
  chromaticity :: RealFloat e => Chromaticity cs i e

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


data Chromaticity cs i e where
  Chromaticity :: Illuminant i =>
    { chromaRed   :: !(Primary e)
    , chromaGreen :: !(Primary e)
    , chromaBlue  :: !(Primary e)
    } -> Chromaticity cs i e
deriving instance Eq e => Eq (Chromaticity cs i e)
deriving instance Elevator e => Show (Chromaticity cs i e) -- TODO: better show with whitepoint

-- | Linear transformation of a pixel in a linear RGB color space into XYZ color space
--
-- ==== __Examples__
--
-- This example depicts the fact that even in @ghci@ when @npm@ is instantiated to a
-- concrete type, despite being derived it is memoized and gets computed only once.
--
-- >>> :set -XDataKinds
-- >>> import Debug.Trace
-- >>> import Graphics.ColorSpace.RGB.Derived.SRGB
-- >>> :{
-- srgbFromLinear :: Pixel (SRGB 'D65) Float -> Pixel XYZ Float
-- srgbFromLinear = npmApply npm'
--   where npm' = trace "Evaluated only once!!!" npm :: NPM (SRGB 'D65) 'D65 Float
-- :}
-- 
-- >>> srgbFromLinear $ PixelRGB 0.1 0.2 0.3
-- <XYZ:(Evaluated only once!!!
--  0.166888, 0.185953, 0.310856)>
-- >>> srgbFromLinear $ PixelRGB 0.1 0.2 0.3
-- <XYZ:( 0.166888, 0.185953, 0.310856)>
-- >>> rgb = PixelRGB 0.1 0.2 0.3 :: Pixel (SRGB 'D65) Float
-- >>> npmApply npm rgb :: Pixel XYZ Float
-- <XYZ:( 0.166888, 0.185953, 0.310856)>
--
-- Here is a comparison with a non-liner sRGB conversion:
--
-- >>> npmApply npm (dcctf rgb) :: Pixel XYZ Float {- non-linear transformation -}
-- <XYZ:( 0.029186, 0.031093, 0.073737)>
-- >>> toPixelXYZ rgb :: Pixel XYZ Float           {- non-linear transformation -}
-- <XYZ:( 0.029186, 0.031093, 0.073737)>
--
--
-- @since 0.1.0
npmApply :: (RedGreenBlue cs i, Elevator e) => NPM cs i e -> Pixel cs e -> Pixel XYZ e
npmApply (NPM npm') = coerce . multM3x3byV3 npm' . coerce . unPixelRGB
{-# INLINE npmApply #-}

-- | Linear transformation of a pixel in XYZ color space into a linear RGB color space
--
-- @since 0.1.0
inpmApply ::
     (RedGreenBlue cs i, Elevator e) => INPM cs i e -> Pixel XYZ e -> Pixel cs e
inpmApply (INPM inpm') = mkPixelRGB . coerce . multM3x3byV3 inpm' . coerce
{-# INLINE inpmApply #-}

-- | Linear transformation of a pixel in a linear luminocity, i.e. the Y component of
-- XYZ color space
npmApplyLuminocity ::
     forall cs i e. (RedGreenBlue cs i, Elevator e, RealFloat e)
  => Pixel cs e
  -> Pixel Y e
npmApplyLuminocity px =
  coerce (m3x3row1 (unNPM (npm :: NPM cs i e)) `dotProduct` coerce (unPixelRGB px))
{-# INLINE npmApplyLuminocity #-}


rgbLuminocity :: (RedGreenBlue cs i, Elevator e, RealFloat e) => Pixel cs e -> Pixel Y e
rgbLuminocity = npmApplyLuminocity . dcctf
{-# INLINE rgbLuminocity #-}

rgb2xyz :: (RedGreenBlue cs i, Elevator e, RealFloat e) => Pixel cs e -> Pixel XYZ e
rgb2xyz = npmApply npm . dcctf
{-# INLINE rgb2xyz #-}

xyz2rgb :: (RedGreenBlue cs i, Elevator e, RealFloat e) => Pixel XYZ e -> Pixel cs e
xyz2rgb = ecctf . inpmApply inpm
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
newtype NPM cs (i :: k) e = NPM
  { unNPM :: M3x3 e
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator e => Show (NPM cs i e) where
  show = show . unNPM

-- | Inverse normalized primary matrix (iNPM), which is used to tranform linear
-- `Graphics.ColorSpace.CIE1931.XYZ.XYZ` color space into a linear RGB color space. It is
-- literally a inverse matrix of `NPM`
--
-- @since 0.1.0
newtype INPM cs (i :: k) e = INPM
  { unINPM :: M3x3 e
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator e => Show (INPM cs i e) where
  show = show . unINPM


-- | Derive a `NPM` form chromaticities and a white point
--
-- @since 0.1.0
npmDerive ::
     forall cs i e. (Elevator e, RealFloat e, Illuminant i)
  => Chromaticity cs i e
  -> NPM cs i e
npmDerive (Chromaticity r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
  where
    !primaries' =
      toRealFloat <$>
      -- transposed matrix with xyz primaries
      M3x3
        (V3 (xPrimary r) (xPrimary g) (xPrimary b))
        (V3 (yPrimary r) (yPrimary g) (yPrimary b))
        (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff = invertM3x3 primaries' `multM3x3byV3` coerce (normalTristimulus :: Tristimulus i e)
{-# INLINE npmDerive #-}

-- | Derive an `INPM` form chromaticities and a white point
--
-- @since 0.1.0
inpmDerive ::
     forall cs i e. (Elevator e, RealFloat e, Illuminant i)
  => Chromaticity cs i e
  -> INPM cs i e
inpmDerive = INPM . invertM3x3 . unNPM . npmDerive
{-# INLINE inpmDerive #-}


-- | Get the `WhitePoint` of chromaticity. `Chromaticity` itself isn't actually evaluated,
-- its type carries enough information for this operation.
--
-- @since 0.1.0
chromaticityWhitePoint :: (RedGreenBlue cs i, RealFloat e) => Chromaticity cs i e -> WhitePoint i e
chromaticityWhitePoint _ = whitePoint
{-# INLINE chromaticityWhitePoint #-}

-- | Get the `Chromaticity` of a pixel in RGB color space. Pixel itself isn't actually
-- evaluated, its type carries enough information for this operation.
--
-- @since 0.1.0
pixelChromaticity :: (RedGreenBlue cs i, RealFloat e) => Pixel cs a -> Chromaticity cs i e
pixelChromaticity _ = chromaticity
{-# INLINE pixelChromaticity #-}


-- | Get the white point of any RGB pixel. Pixel itself isn't evaluated, since its type
-- carries enough information for getting the white point.
--
-- >>> import Graphics.ColorSpace.RGB
-- >>> pixelWhitePoint (PixelRGB8 1 2 3)
-- WhitePointChroma <CIExyY:( 0.312700000000, 0.329000000000)>
--
-- @since 0.1.0
pixelWhitePoint :: (RedGreenBlue cs i, RealFloat e) => Pixel cs a -> WhitePoint i e
pixelWhitePoint _ = whitePoint
{-# INLINE pixelWhitePoint #-}



