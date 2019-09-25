{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.S
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.S
  ( pattern PixelRGB
  , pattern PixelSRGB
  --, ToRGB(..)
  , RGB
  , SRGB
  , computePixelXYZ
  , computePixelRGB
  --, RGBA
  -- , npm
  -- , inpm
  , npmStandard
  , inpmStandard
  , forwardGamma
  , inverseGamma
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.ColorModel.Helpers
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.CIE1931.Illuminants
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Linear



-- | The most common @sRGB@ color space with the default `D65` illuminant
type RGB = SRGB 'D65


-- | The most common @sRGB@ color space
data SRGB (i :: k)


newtype instance Pixel (SRGB i) e = SRGB (Pixel CM.RGB e)
  deriving (Eq, Functor, Applicative, Foldable, Traversable, Storable)

pattern PixelRGB :: e -> e -> e -> Pixel RGB e
pattern PixelRGB r g b = SRGB (CM.PixelRGB r g b)
{-# COMPLETE PixelRGB #-}

pattern PixelSRGB :: e -> e -> e -> Pixel (SRGB i) e
pattern PixelSRGB r g b = SRGB (CM.PixelRGB r g b)
{-# COMPLETE PixelSRGB #-}

-- -- | Conversion to `RGB` color space.
-- class ColorSpace cs e => ToRGB cs e where
--   -- | Convert to an `RGB` pixel.
--   toPixelRGB :: Pixel cs e -> Pixel RGB Double

-- TODO: round to 7 decimal places for floating point
instance Show e => Show (Pixel (SRGB i) e) where
  showsPrec _ (PixelSRGB r g b) =
    showsP "sRGB:" -- ++ show i
    (shows3 r g b)

-- | sRGB defined in 'Graphics.ColorSpace.RGB.S'
instance (Typeable i, Typeable k, Elevator e) => ColorModel (SRGB (i :: k)) e where
  type Components (SRGB i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}


-- TODO: add tex formula
forwardGamma :: Elevator e => Double -> e
forwardGamma u
  | u <= 0.0031308 = fromDouble (12.92 * u)
  | otherwise = fromDouble (1.055 * (u ** (1 / 2.4)) - 0.055)
{-# INLINE forwardGamma #-}

-- TODO: add tex formula
inverseGamma :: Elevator e => e -> Double
inverseGamma eu
  | u <= 0.04045 = u / 12.92
  | otherwise = ((u + 0.055) / 1.055) ** 2.4
  where !u = toDouble eu
{-# INLINE inverseGamma #-}


-- | sRGB with `D65` illuminant
instance Elevator e => ColorSpace RGB e where
  toPixelXYZ = npmApply npmStandard . fmap inverseGamma . coerce
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = coerce . fmap forwardGamma . inpmApply inpmStandard
  {-# INLINE fromPixelXYZ #-}

-- -- | sRGB with `D65` illuminant
-- instance Elevator e => ComputedColorSpace RGB e where
--   toPixelXYZ = npmApply npmStandard . fmap inverseGamma . coerce
--   {-# INLINE toPixelXYZ #-}
--   fromPixelXYZ = coerce . fmap forwardGamma . inpmApply inpmStandard
--   {-# INLINE fromPixelXYZ #-}




-- | Linear sRGB conversion without gamma correction
instance Illuminant i => LinearRGB SRGB i where
  chromaticity = Chromaticity (Primary 0.64 0.33)
                              (Primary 0.30 0.60)
                              (Primary 0.15 0.06)
  mkPixelRGB = coerce
  unPixelRGB = coerce

-- | Convert a pixel in sRGB color space into XYZ color space pixel, while using a
-- computed `NPM` matrix (vs a `npmStandard` matrix with rounded values), as well as
-- applying gamma correction. Source illuminant shall be supplied at a type level.
--
-- >>> import Graphics.ColorSpace.RGB.S
-- >>> import Data.Word
-- >>> :set -XDataKinds
-- >>> computePixelXYZ (PixelSRGB 128 255 0 :: Pixel (SRGB 'D50) Word8)
-- <XYZ:(0.45359299655898744|0.75173107956617|0.12119759448334924)>
-- >>> computePixelXYZ (PixelSRGB 128 255 0 :: Pixel (SRGB 'D65) Word8)
-- <XYZ:(0.44660322355579873|0.7610690409189028|0.1233675399901848)>
--
-- @since 0.1.0
computePixelXYZ ::
     forall i e. (LinearRGB SRGB i, Elevator e)
  => Pixel (SRGB i) e
  -> Pixel XYZ Double
computePixelXYZ = npmApply (npm :: NPM SRGB i) . fmap inverseGamma . unPixelRGB
{-# INLINE computePixelXYZ #-}

-- | Convert a pixel in XYZ color space into sRGB color space pixel, while using a
-- computed `INPM` matrix (vs a `inpmStandard` matrix with rounded values), as well as
-- applying reverse gamma correction. Reslting illuminant shall be supplied at a type
-- level.
--
-- >>> import Graphics.ColorSpace.RGB.S
-- >>> import Data.Word
-- >>> :set -XDataKinds
-- >>> px = PixelSRGB 128 255 0 :: Pixel (SRGB 'D65) Word8
-- >>> computePixelXYZ px
-- <XYZ:(0.44660322355579873|0.7610690409189028|0.1233675399901848)>
-- >>> computePixelRGB (computePixelXYZ px) :: Pixel (SRGB 'D65) Word8
-- <sRGB::(128|255|0)>
--
-- @since 0.1.0
computePixelRGB ::
     forall i e. (LinearRGB SRGB i, Elevator e)
  => Pixel XYZ Double
  -> Pixel (SRGB i) e
computePixelRGB = mkPixelRGB . fmap forwardGamma . inpmApply (inpm :: INPM SRGB i)
{-# INLINE computePixelRGB #-}

convert :: (ColorSpace cs1 e1, ColorSpace cs2 e2) => Pixel cs1 e1 -> Pixel cs2 e2
convert = fromPixelXYZ . toPixelXYZ


-- | Normalized primary matrix, which is used for conversion of linear sRGB with `D65`
-- whitepoint to XYZ, as specified in @IEC 61966-2-1:1999@ standard
--
-- >>> import Graphics.ColorSpace.RGB.S
-- >>> npmStandard
-- [ [ 0.4124000, 0.3576000, 0.1805000]
-- , [ 0.2126000, 0.7152000, 0.0722000]
-- , [ 0.0193000, 0.1192000, 0.9505000] ]
-- >>> extractWhitePoint SRGB.npmStandard
-- WhitePoint {xWhitePoint = 0.3127, yWhitePoint = 0.329}
--
-- If you prefer to use a matrix with values that wheren't rounded, like it is above, you
-- can use the computed `npm` instead (below rounding is done only during conversion to
-- string):
--
-- >>> :set -XDataKinds
-- >>> npm :: NPM SRGB.RGB 'D65
-- [ [ 0.4123908, 0.3575843, 0.1804808]
-- , [ 0.2126390, 0.7151687, 0.0721923]
-- , [ 0.0193308, 0.1191948, 0.9505322] ]
--
-- @since 0.1.0
npmStandard :: NPM SRGB 'D65
npmStandard = NPM $ M3x3 (V3 0.4124 0.3576 0.1805)
                         (V3 0.2126 0.7152 0.0722)
                         (V3 0.0193 0.1192 0.9505)

-- | Inverse of normalized primary matrix, which is used for conversion of XYZ to linear
-- sRGB, as specified in @IEC 61966-2-1:1999@ standard
--
-- >>> import Graphics.ColorSpace.RGB.S
-- >>> inpmStandard
-- [ [ 3.2406000,-1.5372000,-0.4986000]
-- , [-0.9689000, 1.8758000, 0.0415000]
-- , [ 0.0557000,-0.2040000, 1.0570000] ]
--
-- @since 0.1.0
inpmStandard :: INPM SRGB 'D65
inpmStandard = INPM $ M3x3 (V3  3.2406 -1.5372 -0.4986)
                           (V3 -0.9689  1.8758  0.0415)
                           (V3  0.0557 -0.2040  1.0570)



-- m3x3' :: Elevator e => (e -> e -> e -> b) -> V3 -> V3 -> V3 -> V3 -> b
-- m3x3' f (V3 m00 m01 m02) (V3 m10 m11 m12) (V3 m20 m21 m22) (V3 v0 v1 v2) =
--   f (fromDouble (m00 * v0 + m01 * v0 + m02 * v0))
--     (fromDouble (m10 * v1 + m11 * v1 + m12 * v1))
--     (fromDouble (m20 * v2 + m21 * v2 + m22 * v2))
-- {-# INLINE m3x3' #-}


-- instance Functor (Pixel RGB) where
--   fmap f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
--   {-# INLINE fmap #-}

-- instance Applicative (Pixel RGB) where
--   pure !e = PixelRGB e e e
--   {-# INLINE pure #-}
--   (PixelRGB fr fg fb) <*> (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
--   {-# INLINE (<*>) #-}

-- instance Foldable (Pixel RGB) where
--   foldr f !z (PixelRGB r g b) = foldr3 f z r g b
--   {-# INLINE foldr #-}

-- instance Traversable (Pixel RGB) where
--   traverse f (PixelRGB r g b) = traverse3 PixelRGB f r g b
--   {-# INLINE traverse #-}

-- instance Storable e => Storable (Pixel RGB e) where
--   sizeOf = sizeOfN 3
--   {-# INLINE sizeOf #-}
--   alignment = alignmentN 3
--   {-# INLINE alignment #-}
--   peek = peek3 PixelRGB
--   {-# INLINE peek #-}
--   poke p (PixelRGB r g b) = poke3 p r g b
--   {-# INLINE poke #-}

------------
--- RGBA ---
------------


-- -- | @sRGB@ color space with Alpha channel.
-- data RGBA


-- data instance Pixel RGBA e = PixelRGBA !e !e !e !e deriving (Eq, Ord)


-- instance Show e => Show (Pixel RGBA e) where
--   showsPrec _ (PixelRGBA r g b a) = showsP "sRGBA" (shows4 r g b a)


-- instance Elevator e => ColorSpace RGBA e where
--   type Components RGBA e = (e, e, e, e)

--   toComponents (PixelRGBA r g b a) = (r, g, b, a)
--   {-# INLINE toComponents #-}
--   fromComponents (r, g, b, a) = PixelRGBA r g b a
--   {-# INLINE fromComponents #-}

-- instance Elevator e => AlphaSpace RGBA e where
--   type Opaque RGBA = RGB
--   getAlpha (PixelRGBA _ _ _ a) = a
--   {-# INLINE getAlpha #-}
--   addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
--   {-# INLINE addAlpha #-}
--   dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
--   {-# INLINE dropAlpha #-}


-- instance Functor (Pixel RGBA) where
--   fmap f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
--   {-# INLINE fmap #-}

-- instance Applicative (Pixel RGBA) where
--   pure !e = PixelRGBA e e e e
--   {-# INLINE pure #-}
--   (PixelRGBA fr fg fb fa) <*> (PixelRGBA r g b a) = PixelRGBA (fr r) (fg g) (fb b) (fa a)
--   {-# INLINE (<*>) #-}

-- instance Foldable (Pixel RGBA) where
--   foldr f !z (PixelRGBA r g b a) = foldr4 f z r g b a
--   {-# INLINE foldr #-}

-- instance Traversable (Pixel RGBA) where
--   traverse f (PixelRGBA r g b a) = traverse4 PixelRGBA f r g b a
--   {-# INLINE traverse #-}

-- instance Storable e => Storable (Pixel RGBA e) where
--   sizeOf = sizeOfN 4
--   {-# INLINE sizeOf #-}
--   alignment = alignmentN 4
--   {-# INLINE alignment #-}
--   peek = peek4 PixelRGBA
--   {-# INLINE peek #-}
--   poke p (PixelRGBA r g b a) = poke4 p r g b a
--   {-# INLINE poke #-}
