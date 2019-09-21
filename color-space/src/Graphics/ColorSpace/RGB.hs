{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB (
  RGB, RGBA, Pixel(..), ToRGB(..)
  ) where

import Foreign.Storable
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.Algebra
import Prelude hiding (map)


-----------
--- RGB ---
-----------

-- rgb2xyzM3x3 :: Chromaticity -> M3x3
-- rgb2xyzM3x3 Chromaticity {..} =
--   M3x3
--     (V3 (rS * rX) (gS * gX) (bS * bX))
--     (V3 (rS * rY) (gS * gY) (bS * bY))
--     (V3 (rS * rZ) (gS * gZ) (bS * bZ))
--   where
--     V3 rX rY rZ = primaryXYZ chromaRed
--     V3 gX gY gZ = primaryXYZ chromaGreen
--     V3 bX bY bZ = primaryXYZ chromaBlue
--     V3 rS gS bS = invertM3x3 (M3x3 (V3 rX gX bX)
--                                    (V3 rY gY bY)
--                                    (V3 rZ gZ bZ)) `multM3x3byV3` whitePointXYZ chromaWhite
-- {-# INLINE rgb2xyzM3x3 #-}



srgb2xyzM3x3 :: Chromaticity -> M3x3
srgb2xyzM3x3 (Chromaticity r g b w) = primaries' * M3x3 coeff coeff coeff
  where
    -- transposed matrix with xyz primaries
    primaries' = M3x3 (V3 (xPrimary r) (xPrimary g) (xPrimary b))
                      (V3 (yPrimary r) (yPrimary g) (yPrimary b))
                      (V3 (zPrimary r) (zPrimary g) (zPrimary b))
    wXYZ = whitePointXYZ w
    coeff = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ w

    --V3 cr cg cb = invertM3x3 primaries' `multM3x3byV3` whitePointXYZ w
    -- V3 xr yr zr = primaryXYZ chromaRed
    -- V3 gX gY gZ = primaryXYZ chromaGreen
    -- V3 bX bY bZ = primaryXYZ chromaBlue
    -- V3 rS gS bS = invertM3x3 (M3x3 (V3 rX gX bX)
    --                                (V3 rY gY bY)
    --                                (V3 rZ gZ bZ)) `multM3x3byV3` whitePointXYZ chromaWhite
{-# INLINE srgb2xyzM3x3 #-}


d50 = WhitePoint 0.34570 0.35850
d65 = WhitePoint 0.31270 0.32900

d65XYZ :: V3
d65XYZ = whitePointXYZ d65


sRGBchromaticity =
  Chromaticity
    (Primary 0.64 0.33)
    (Primary 0.30 0.60)
    (Primary 0.15 0.06)
    d65

adobeRGBchromaticity =
  Chromaticity
    (Primary 0.64 0.33)
    (Primary 0.21 0.71)
    (Primary 0.15 0.06)
    d65


-- | The most common @sRGB@ color space
data RGB

data instance Pixel RGB e = PixelRGB !e !e !e deriving (Eq, Ord)


data AdobeRGB


newtype instance Pixel AdobeRGB e = PixelAdobeRGB (Pixel RGB e)
  deriving (Eq, Show, Functor, Applicative, Foldable, Traversable, Storable)

pattern PixelAdRGB :: e -> e -> e -> Pixel AdobeRGB e
pattern PixelAdRGB r g b = PixelAdobeRGB (PixelRGB r g b)

-- | Conversion to `RGB` color space.
class ColorSpace cs e => ToRGB cs e where
  -- | Convert to an `RGB` pixel.
  toPixelRGB :: Pixel cs e -> Pixel RGB Double


instance Show e => Show (Pixel RGB e) where
  showsPrec _ (PixelRGB r g b) = showsP "sRGB" (shows3 r g b)

instance Elevator e => ColorSpace RGB e where
  type Components RGB e = (e, e, e)
  toComponents (PixelRGB r g b) = (r, g, b)
  {-# INLINE toComponents #-}
  fromComponents (r, g, b) = PixelRGB r g b
  {-# INLINE fromComponents #-}
  toPixelXYZ (PixelRGB r g b) =
    m3x3'
      PixelXYZ
      (V3 0.4124 0.3576 0.1805)
      (V3 0.2126 0.7152 0.0722)
      (V3 0.0193 0.1192 0.9505)
      (toV3 r g b)
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ (PixelXYZ x y z) =
    m3x3'
      PixelRGB
      (V3  3.2406 -1.5372 -0.4986)
      (V3 -0.9689  1.8758  0.0415)
      (V3  0.0557 -0.2040  1.0570)
      (V3 x y z)
  {-# INLINE fromPixelXYZ #-}

-- | Normalized primary matrix Matrix for conversion of linear sRGB to XYZ
npm = M3x3 (V3 0.41239079926595923 0.3575843393838781 0.1804807884018344)
           (V3 0.21263900587151022 0.7151686787677562 0.0721923153607337)
           (V3 0.01933081871559182 0.1191947797946260 0.9505321522496610)

toV3 :: Elevator e => e -> e -> e -> V3
toV3 v0 v1 v2 = V3 (eToDouble v0) (eToDouble v1) (eToDouble v2)

m3x3' :: Elevator e => (e -> e -> e -> b) -> V3 -> V3 -> V3 -> V3 -> b
m3x3' f (V3 m00 m01 m02) (V3 m10 m11 m12) (V3 m20 m21 m22) (V3 v0 v1 v2) =
  f (eFromDouble (m00 * v0 + m01 * v0 + m02 * v0))
    (eFromDouble (m10 * v1 + m11 * v1 + m12 * v1))
    (eFromDouble (m20 * v2 + m21 * v2 + m22 * v2))
{-# INLINE m3x3' #-}


instance Functor (Pixel RGB) where
  fmap f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE fmap #-}

instance Applicative (Pixel RGB) where
  pure !e = PixelRGB e e e
  {-# INLINE pure #-}
  (PixelRGB fr fg fb) <*> (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel RGB) where
  foldr f !z (PixelRGB r g b) = foldr3 f z r g b
  {-# INLINE foldr #-}

instance Traversable (Pixel RGB) where
  traverse f (PixelRGB r g b) = traverse3 PixelRGB f r g b
  {-# INLINE traverse #-}

instance Storable e => Storable (Pixel RGB e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelRGB
  {-# INLINE peek #-}
  poke p (PixelRGB r g b) = poke3 p r g b
  {-# INLINE poke #-}

------------
--- RGBA ---
------------


-- | @sRGB@ color space with Alpha channel.
data RGBA


data instance Pixel RGBA e = PixelRGBA !e !e !e !e deriving (Eq, Ord)


instance Show e => Show (Pixel RGBA e) where
  showsPrec _ (PixelRGBA r g b a) = showsP "sRGBA" (shows4 r g b a)


instance Elevator e => ColorSpace RGBA e where
  type Components RGBA e = (e, e, e, e)

  toComponents (PixelRGBA r g b a) = (r, g, b, a)
  {-# INLINE toComponents #-}
  fromComponents (r, g, b, a) = PixelRGBA r g b a
  {-# INLINE fromComponents #-}

instance Elevator e => AlphaSpace RGBA e where
  type Opaque RGBA = RGB
  getAlpha (PixelRGBA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
  {-# INLINE addAlpha #-}
  dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
  {-# INLINE dropAlpha #-}


instance Functor (Pixel RGBA) where
  fmap f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
  {-# INLINE fmap #-}

instance Applicative (Pixel RGBA) where
  pure !e = PixelRGBA e e e e
  {-# INLINE pure #-}
  (PixelRGBA fr fg fb fa) <*> (PixelRGBA r g b a) = PixelRGBA (fr r) (fg g) (fb b) (fa a)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel RGBA) where
  foldr f !z (PixelRGBA r g b a) = foldr4 f z r g b a
  {-# INLINE foldr #-}

instance Traversable (Pixel RGBA) where
  traverse f (PixelRGBA r g b a) = traverse4 PixelRGBA f r g b a
  {-# INLINE traverse #-}

instance Storable e => Storable (Pixel RGBA e) where
  sizeOf = sizeOfN 4
  {-# INLINE sizeOf #-}
  alignment = alignmentN 4
  {-# INLINE alignment #-}
  peek = peek4 PixelRGBA
  {-# INLINE peek #-}
  poke p (PixelRGBA r g b a) = poke4 p r g b a
  {-# INLINE poke #-}
