{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RedGreenBlue.Derived.SRGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RedGreenBlue.Derived.SRGB
  ( pattern PixelRGB
  , RGB
  , SRGB
  , Pixel(RGB)
  --, RGBA
  , SRGB.primaries
  , SRGB.transfer
  , SRGB.itransfer
  ) where

import Data.Typeable
import Data.Coerce
import Foreign.Storable
import Graphics.ColorModel.Helpers
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import qualified Graphics.ColorSpace.RedGreenBlue.SRGB as SRGB
import Graphics.ColorSpace.CIE1931.Illuminants
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RedGreenBlue.Internal



type SRGB = RGB 'D65


-- | The most common @sRGB@ color space with the default `D65` illuminant
data RGB (i :: k)

newtype instance Pixel (RGB i) e = RGB (Pixel CM.RGB e)
  deriving (Eq, Functor, Applicative, Foldable, Traversable, Storable)

-- | Constructor for the most common @sRGB@ color space with the default `D65` illuminant
pattern PixelRGB :: e -> e -> e -> Pixel (RGB i) e
pattern PixelRGB r g b = RGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}

-- TODO: round to 7 decimal places for floating point
instance (Illuminant i, Show e) => Show (Pixel (RGB i) e) where
  showsPrec _ (PixelRGB r g b) =
    showsP "sRGB:" -- ++ show i
    (shows3 r g b)

-- | sRGB defined in 'Graphics.ColorSpace.RGB.S'
instance (Typeable i, Typeable k, Elevator e) => ColorModel (RGB (i :: k)) e where
  type Components (RGB i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}

-- | sRGB color space
instance (Illuminant i, Typeable i, Typeable k, Elevator e) => ColorSpace (RGB (i :: k)) e where
  type BaseColorSpace (RGB i) = RGB i
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = xyz2rgb
  {-# INLINE fromPixelXYZ #-}


instance Illuminant i => RedGreenBlue RGB i where
  chromaticity = SRGB.primaries
  ecctf = fmap SRGB.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap SRGB.itransfer
  {-# INLINE dcctf #-}

-- vGamutNPM = M3x3 (V3  0.679644  0.152211  0.118600)
--                  (V3  0.260686  0.774894 -0.035580)
--                  (V3 -0.009310 -0.004612  1.102980)

-- vGamutINPM = M3x3 (V3  1.589012 -0.313204 -0.180965)
--                   (V3 -0.534053  1.396011  0.102458)
--                   (V3  0.011179  0.003194  0.905535)

-- acesNPM = M3x3 (V3 0.9525523959 0.0000000000  0.0000936786)
--                (V3 0.3439664498 0.7281660966 -0.0721325464)
--                (V3 0.0000000000 0.0000000000  1.0088251844)

-- acesINPM = M3x3 (V3  1.0498110175 0.0000000000 -0.0000974845)
--                 (V3 -0.4959030231 1.3733130458  0.0982400361)
--                 (V3  0.0000000000 0.0000000000  0.9912520182)
-- -- | Convert a pixel in sRGB color space into XYZ color space pixel, by first applying
-- -- gamma correction function and then using a computed `NPM` matrix (vs a `npmStandard`
-- -- matrix with rounded values).
-- --
-- -- >>> import Graphics.ColorSpace.RGB.S
-- -- >>> import Data.Word
-- -- >>> :set -XDataKinds
-- -- >>> computePixelXYZ (PixelRGB 128 255 0 :: Pixel (RGB 'D50) Word8)
-- -- <XYZ:(0.45359299655898744|0.75173107956617|0.12119759448334924)>
-- -- >>> computePixelXYZ (PixelRGB 128 255 0 :: Pixel (RGB i) Word8)
-- -- <XYZ:(0.44660322355579873|0.7610690409189028|0.1233675399901848)>
-- --
-- -- @since 0.1.0
-- computePixelXYZ ::
--      forall i e. (DerivedRGB RGB i, Elevator e)
--   => Pixel (RGB i) e
--   -> Pixel XYZ Double
-- computePixelXYZ = npmApply (npm :: NPM RGB i) . fmap itransfer . unPixelRGB
-- {-# INLINE computePixelXYZ #-}

-- -- | Similar to `computePixelXYZ`, but in the opposite firection. Convert a pixel in XYZ
-- -- color space into sRGB color space pixel, while using a computed `INPM` matrix (vs a
-- -- `inpmStandard` matrix with rounded values), followed by applying reverse gamma
-- -- correction. Reslting illuminant is required to be supplied at the type level.
-- --
-- -- >>> import Graphics.ColorSpace.RGB.S
-- -- >>> import Data.Word
-- -- >>> :set -XDataKinds
-- -- >>> px = PixelSRGB 128 255 0 :: Pixel (SRGB i) Word8
-- -- >>> computePixelXYZ px
-- -- <XYZ:(0.44660322355579873|0.7610690409189028|0.1233675399901848)>
-- -- >>> computePixelRGB (computePixelXYZ px) :: Pixel (SRGB i) Word8
-- -- <sRGB::(128|255|0)>
-- --
-- -- @since 0.1.0
-- computePixelRGB ::
--      forall i e. (DerivedRGB RGB i, Elevator e)
--   => Pixel XYZ Double
--   -> Pixel (RGB i) e
-- computePixelRGB = mkPixelRGB . fmap transfer . inpmApply (inpm :: INPM RGB i)
-- {-# INLINE computePixelRGB #-}

convert :: (ColorSpace cs' e', ColorSpace cs e) => Pixel cs' e' -> Pixel cs e
convert = fromPixelXYZ . toPixelXYZ



-- -- | Normalized primary matrix, which is what we use for conversion of linear sRGB with `D65`
-- -- whitepoint to XYZ, as specified in @IEC 61966-2-1:1999@ standard
-- --
-- -- >>> import Graphics.ColorSpace.RGB.S
-- -- >>> npmStandard
-- -- [ [ 0.4124000, 0.3576000, 0.1805000]
-- -- , [ 0.2126000, 0.7152000, 0.0722000]
-- -- , [ 0.0193000, 0.1192000, 0.9505000] ]
-- -- >>> extractWhitePoint SRGB.npmStandard
-- -- WhitePoint {xWhitePoint = 0.3127, yWhitePoint = 0.329}
-- --
-- -- If you prefer to use a matrix with values that wheren't rounded, like it is above, you
-- -- can use the computed `npm` instead (below rounding is done only during conversion to
-- -- string):
-- --
-- -- >>> :set -XDataKinds
-- -- >>> npm :: NPM SRGB.RGB i
-- -- [ [ 0.4123908, 0.3575843, 0.1804808]
-- -- , [ 0.2126390, 0.7151687, 0.0721923]
-- -- , [ 0.0193308, 0.1191948, 0.9505322] ]
-- --
-- -- @since 0.1.0
-- npmSRGB :: NPM RGB i
-- npmSRGB = NPM $ M3x3 (V3 0.4124 0.3576 0.1805)
--                      (V3 0.2126 0.7152 0.0722)
--                      (V3 0.0193 0.1192 0.9505)

-- -- | Inverse of normalized primary matrix, which is what we use for conversion of XYZ to linear
-- -- sRGB, as specified in @IEC 61966-2-1:1999@ standard
-- --
-- -- >>> import Graphics.ColorSpace.RGB.S
-- -- >>> inpmStandard
-- -- [ [ 3.2406000,-1.5372000,-0.4986000]
-- -- , [-0.9689000, 1.8758000, 0.0415000]
-- -- , [ 0.0557000,-0.2040000, 1.0570000] ]
-- --
-- -- @since 0.1.0
-- inpmSRGB :: INPM RGB i
-- inpmSRGB = INPM $ M3x3 (V3  3.2406 -1.5372 -0.4986)
--                        (V3 -0.9689  1.8758  0.0415)
--                        (V3  0.0557 -0.2040  1.0570)

