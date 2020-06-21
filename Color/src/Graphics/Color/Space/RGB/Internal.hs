{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Internal
  ( pattern ColorRGB
  , pattern ColorRGBA
  , RedGreenBlue(..)
  , Linearity(..)
  , Gamut(..)
  , rgb2xyz
  , rgbLinear2xyz
  , xyz2rgb
  , xyz2rgbLinear
  , rgbLuminance
  , rgbLinearLuminance
  , NPM(..)
  , npmApply
  , npmDerive
  , INPM(..)
  , inpmApply
  , inpmDerive
  , rgbColorGamut
  , pixelWhitePoint
  , gamutWhitePoint
  , module Graphics.Color.Algebra
  ) where

import Data.Coerce
import Graphics.Color.Algebra
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space.Internal
import Data.Kind

data Linearity = Linear | NonLinear

class Illuminant i => RedGreenBlue (cs :: Linearity -> Type) (i :: k) | cs -> i where
  -- | RGB primaries that are defined for the RGB color space, while point is defined by
  -- the __@i@__ type parameter
  gamut :: RealFloat e => Gamut cs i e

  -- | Encoding color component transfer function (inverse). Also known as opto-electronic
  -- transfer function (OETF / OECF) or gamma function.
  ecctf :: (RealFloat a, Elevator a) => Color (cs 'Linear) a -> Color (cs 'NonLinear) a

  -- | Decoding color component transfer function (forward)
  dcctf :: (RealFloat a, Elevator a) => Color (cs 'NonLinear) a -> Color (cs 'Linear) a

  -- | Normalized primary matrix for this RGB color space. Default implementation derives
  -- it from `chromaticity`
  npm :: (ColorSpace (cs 'Linear) i a, RealFloat a) => NPM cs a
  npm = npmDerive gamut
  {-# INLINE npm #-}

  -- | Inverse normalized primary matrix for this RGB color space. Default implementation
  -- derives it from `chromaticity`
  inpm :: (ColorSpace (cs 'Linear) i a, RealFloat a) => INPM cs a
  inpm = inpmDerive gamut
  {-# INLINE inpm #-}

  -- | Lift RGB color model into a RGB color space
  mkColorRGB :: Color CM.RGB e -> Color (cs l) e
  default mkColorRGB ::
    Coercible (Color CM.RGB e) (Color (cs l) e) => Color CM.RGB e -> Color (cs l) e
  mkColorRGB = coerce

  -- | Drop RGB color space down to the RGB color model
  unColorRGB :: Color (cs l) e -> Color CM.RGB e
  default unColorRGB ::
    Coercible (Color (cs l) e) (Color CM.RGB e) => Color (cs l) e -> Color CM.RGB e
  unColorRGB = coerce


data Gamut cs i e = Gamut
  { gamutRedPrimary   :: !(Primary i e)
  , gamutGreenPrimary :: !(Primary i e)
  , gamutBluePrimary  :: !(Primary i e)
  }
deriving instance Eq e => Eq (Gamut cs i e)
deriving instance ColorSpace cs i e => Show (Gamut cs i e) -- TODO: better show with whitepoint

-- | Get the `WhitePoint` of chromaticity. `Chromaticity` itself isn't actually evaluated,
-- its type carries enough information for this operation.
--
-- @since 0.1.0
gamutWhitePoint :: (RedGreenBlue cs i, RealFloat e) => Gamut cs i e -> WhitePoint i e
gamutWhitePoint _ = whitePoint
{-# INLINE gamutWhitePoint #-}


-- | Linear transformation of a pixel in a linear RGB color space into XYZ color space
--
-- ==== __Examples__
--
-- This example depicts the fact that even in @ghci@ when @npm@ is instantiated to a
-- concrete type, despite being derived it is memoized and gets computed only once.
--
-- >>> :set -XDataKinds
-- >>> import Debug.Trace
-- >>> import Graphics.Color.Illuminant.CIE1931
-- >>> import Graphics.Color.Space.RGB.Derived.SRGB
-- >>> :{
-- srgbFromLinear :: Color (SRGB 'D65 'Linear) Float -> Color (XYZ 'D65) Float
-- srgbFromLinear = npmApply npm'
--   where npm' = trace "Evaluated only once!!!" npm :: NPM (SRGB 'D65) Float
-- :}
--
-- >>> srgbFromLinear $ ColorRGB 0.1 0.2 0.3
-- <XYZ CIE1931 'D65:(Evaluated only once!!!
--  0.166888, 0.185953, 0.310856)>
-- >>> srgbFromLinear $ ColorRGB 0.1 0.2 0.3
-- <XYZ CIE1931 'D65:( 0.166888, 0.185953, 0.310856)>
-- >>> rgb = ColorRGB 0.1 0.2 0.3 :: Color (SRGB 'D65 'Linear) Float
-- >>> npmApply npm rgb :: Color (XYZ 'D65) Float
-- <XYZ CIE1931 'D65:( 0.166888, 0.185953, 0.310856)>
--
-- Here is a comparison with a non-liner sRGB conversion:
--
-- >>> rgb = ColorRGB 0.1 0.2 0.3 :: Color (SRGB 'D65 'NonLinear) Float
-- >>> npmApply npm (dcctf rgb) :: Color (XYZ 'D65) Float {- non-linear transformation -}
-- <XYZ CIE1931 'D65:( 0.029186, 0.031093, 0.073737)>
-- >>> toColorXYZ rgb :: Color (XYZ 'D65) Float           {- non-linear transformation -}
-- <XYZ CIE1931 'D65:( 0.029186, 0.031093, 0.073737)>
--
--
-- @since 0.1.0
npmApply ::
     (RedGreenBlue cs i, Elevator e)
  => NPM cs e
  -> Color (cs 'Linear) e
  -> Color (XYZ i) e
npmApply (NPM npm') = coerce . multM3x3byV3 npm' . coerce . unColorRGB
{-# INLINE npmApply #-}

-- | Linear transformation of a pixel in XYZ color space into a linear RGB color space
--
-- @since 0.1.0
inpmApply ::
     (RedGreenBlue cs i, Elevator e)
  => INPM cs e
  -> Color (XYZ i) e
  -> Color (cs 'Linear) e
inpmApply (INPM inpm') = mkColorRGB . coerce . multM3x3byV3 inpm' . coerce
{-# INLINE inpmApply #-}

-- | Linear transformation of a color into a linear luminance, i.e. the Y component of
-- XYZ color space
rgbLinearLuminance ::
     forall cs i e. (RedGreenBlue cs i, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (cs 'Linear) e
  -> Color (Y i) e
rgbLinearLuminance px =
  Y (m3x3row1 (unNPM (npm :: NPM cs e)) `dotProduct` coerce (unColorRGB px))
{-# INLINE rgbLinearLuminance #-}


rgbLuminance ::
     (RedGreenBlue cs i, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (cs 'NonLinear) e
  -> Color (Y i) e
rgbLuminance = rgbLinearLuminance . dcctf
{-# INLINE rgbLuminance #-}

rgb2xyz ::
     (RedGreenBlue cs i, ColorSpace (cs 'NonLinear) i e, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (cs 'NonLinear) e
  -> Color (XYZ i) e
rgb2xyz = npmApply npm . dcctf
{-# INLINE rgb2xyz #-}

xyz2rgb ::
     (RedGreenBlue cs i, ColorSpace (cs 'NonLinear) i e, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (XYZ i) e
  -> Color (cs 'NonLinear) e
xyz2rgb = ecctf . inpmApply inpm
{-# INLINE xyz2rgb #-}


rgbLinear2xyz ::
     (RedGreenBlue cs i, ColorSpace (cs 'NonLinear) i e, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (cs 'Linear) e
  -> Color (XYZ i) e
rgbLinear2xyz = npmApply npm
{-# INLINE rgbLinear2xyz #-}

xyz2rgbLinear ::
     forall cs i e.
     (RedGreenBlue cs i, ColorSpace (cs 'NonLinear) i e, ColorSpace (cs 'Linear) i e, RealFloat e)
  => Color (XYZ i) e
  -> Color (cs 'Linear) e
xyz2rgbLinear = inpmApply inpm
{-# INLINE xyz2rgbLinear #-}

-- | Constructor for an RGB color space.
pattern ColorRGB :: RedGreenBlue cs i => e -> e -> e -> Color (cs l) e
pattern ColorRGB r g b <- (unColorRGB -> CM.ColorRGB r g b) where
        ColorRGB r g b = mkColorRGB (CM.ColorRGB r g b)
{-# COMPLETE ColorRGB #-}

-- | Constructor for an RGB color space with Alpha channel
pattern ColorRGBA :: RedGreenBlue cs i => e -> e -> e -> e -> Color (Alpha (cs l)) e
pattern ColorRGBA r g b a <- Alpha (unColorRGB -> CM.ColorRGB r g b) a where
        ColorRGBA r g b a = Alpha (mkColorRGB (CM.ColorRGB r g b)) a
{-# COMPLETE ColorRGBA #-}


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
--   -> Color (cs1 i1) e1
--   -> Color (cs2 i2) e2
-- applyConversionMatrix (ConversionMatrix m) px =
--   mkColorRGB $ fromV3 CM.ColorRGB $ multM3x3byV3 m (toV3 r g b)
--   where
--     CM.ColorRGB r g b = unColorRGB px

-- | Normalized primary matrix (NPM), which is used to tranform linear RGB color space
-- into `Graphics.Color.Space.CIE1931.XYZ.XYZ` color space.
--
-- @since 0.1.0
newtype NPM cs e = NPM
  { unNPM :: M3x3 e
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator e => Show (NPM cs e) where
  show = show . unNPM

-- | Inverse normalized primary matrix (iNPM), which is used to tranform linear
-- `Graphics.Color.Space.CIE1931.XYZ.XYZ` color space into a linear RGB color space. It is
-- literally a inverse matrix of `NPM`
--
-- @since 0.1.0
newtype INPM cs e = INPM
  { unINPM :: M3x3 e
  } deriving (Eq, Functor, Applicative, Foldable, Traversable)

instance Elevator e => Show (INPM cs e) where
  show = show . unINPM


-- | Derive a `NPM` form chromaticities and a white point
--
-- @since 0.1.0
npmDerive ::
     forall cs i e. (ColorSpace (cs 'Linear) i e, RealFloat e)
  => Gamut cs i e
  -> NPM cs e
npmDerive (Gamut r g b) = NPM (primaries' * M3x3 coeff coeff coeff)
  where
    !primaries' =
      toRealFloat <$>
      -- transposed matrix with xyz primaries
      M3x3
        (V3 (xPrimary r) (xPrimary g) (xPrimary b))
        (V3 (yPrimary r) (yPrimary g) (yPrimary b))
        (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    !coeff = invertM3x3 primaries' `multM3x3byV3` coerce (whitePointTristimulus :: Color (XYZ i) e)
{-# INLINE npmDerive #-}

-- | Derive an `INPM` form chromaticities and a white point
--
-- @since 0.1.0
inpmDerive ::
     forall cs i e. (ColorSpace (cs 'Linear) i e, RealFloat e)
  => Gamut cs i e
  -> INPM cs e
inpmDerive = INPM . invertM3x3 . unNPM . npmDerive
{-# INLINE inpmDerive #-}



-- | Get the `Chromaticity` of a pixel in RGB color space. Color itself isn't actually
-- evaluated, its type carries enough information for this operation.
--
-- @since 0.1.0
rgbColorGamut :: (RedGreenBlue cs i, RealFloat e) => Color (cs l) a -> Gamut cs i e
rgbColorGamut _ = gamut
{-# INLINE rgbColorGamut #-}


-- | Get the white point of any RGB pixel. Color itself isn't evaluated, since its type
-- carries enough information for getting the white point.
--
-- >>> import Graphics.Color.Space.RGB
-- >>> :set -XTypeApplications
-- >>> pixelWhitePoint @Float (ColorSRGB @Word8 1 2 3)
-- WhitePoint (Chromaticity <CIExyY * D65:( 0.312700, 0.329000)>)
-- >>> Just (pixelWhitePoint @Float (ColorSRGB @Word8 1 2 3))
-- Just (WhitePoint (Chromaticity <CIExyY * D65:( 0.312700, 0.329000)>))
--
-- @since 0.1.0
pixelWhitePoint ::
     forall e cs a i l. (RedGreenBlue cs i, RealFloat e)
  => Color (cs l) a
  -> WhitePoint i e
pixelWhitePoint _ = whitePoint
{-# INLINE pixelWhitePoint #-}



