{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Alternative.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Alternative.YCbCr
  ( pattern PixelYCbCr
  , pattern PixelYCbCrA
  , YCbCr
  , Pixel(YCbCr)
  , module Graphics.ColorSpace
  ) where

import Data.Coerce
import Foreign.Storable
import Graphics.ColorModel.Alpha
import Graphics.ColorModel.Internal
import Graphics.ColorModel.Y
import qualified Graphics.ColorModel.YCbCr as CM
import Graphics.ColorSpace
import Graphics.ColorSpace.RGB.SRGB
import Graphics.ColorSpace.RGB.Luma

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
data YCbCr cs

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Pixel (YCbCr cs) e = YCbCr (Pixel CM.YCbCr e)

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Pixel (YCbCr cs) e)
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Pixel (YCbCr cs) e)
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Pixel (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Pixel (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Pixel (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Pixel (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Pixel (YCbCr cs) e)

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Pixel (YCbCr cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative YCbCr color model
pattern PixelYCbCr :: e -> e -> e -> Pixel (YCbCr cs) e
pattern PixelYCbCr y cb cr = YCbCr (CM.PixelYCbCr y cb cr)
{-# COMPLETE PixelYCbCr #-}

-- | Constructor for @YCbCr@ with alpha channel.
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha (YCbCr cs)) e
pattern PixelYCbCrA y cb cr a = Alpha (YCbCr (CM.PixelYCbCr y cb cr)) a
{-# COMPLETE PixelYCbCrA #-}


-- | `YCbCr` color model
instance ColorModel cs e => ColorModel (YCbCr cs) e where
  type Components (YCbCr cs) e = (e, e, e)
  toComponents (PixelYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = PixelYCbCr y cb cr
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("YCbCr-" ++) . showsColorModelName (pure 0 :: Pixel cs e)

-- | `YCbCr` representation for `SRGB` color space
instance Elevator e => ColorSpace (YCbCr SRGB) D65 e where
  type BaseColorSpace (YCbCr SRGB) = SRGB
  toBaseColorSpace = fmap fromRealFloat . ycbcr2rgb . fmap toFloat
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = fmap fromRealFloat . rgb2ycbcr . fmap toFloat
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
instance (Luma (cs i), ColorSpace (cs i) i e, RedGreenBlue (cs i) i) =>
         ColorSpace (YCbCr (cs i)) i e where
  type BaseColorSpace (YCbCr (cs i)) = cs i
  toBaseColorSpace = fmap fromDouble . fromPixelYCbCr
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = fmap fromDouble . toPixelYCbCr
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}




-- | Source: ITU-T Rec. T.871
ycbcr2rgb :: (RedGreenBlue cs i, Ord e, Floating e) => Pixel (YCbCr cs) e -> Pixel cs e
ycbcr2rgb (PixelYCbCr y cb cr) = PixelRGB r g b
  where
    !cb05 = cb - 0.5
    !cr05 = cr - 0.5
    !r = clamp01 (y                   + 1.402    * cr05)
    !g = clamp01 (y - 0.344136 * cb05 - 0.714136 * cr05)
    !b = clamp01 (y + 1.772    * cb05)
{-# INLINE ycbcr2rgb #-}

-- | Source: ITU-T Rec. T.871
rgb2ycbcr :: (RedGreenBlue cs i, Floating e) => Pixel cs e -> Pixel (YCbCr cs) e
rgb2ycbcr (PixelRGB r g b) = PixelYCbCr y cb cr
  where
    !y  =          0.299 * r +    0.587 * g +    0.114 * b
    !cb = 0.5 - 0.168736 * r - 0.331264 * g +      0.5 * b
    !cr = 0.5 +      0.5 * r - 0.418688 * g - 0.081312 * b
{-# INLINE rgb2ycbcr #-}


toPixelYCbCr ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Pixel cs e'
  -> Pixel (YCbCr cs) e
toPixelYCbCr rgb = YCbCr (CM.rgb2ycbcr (unPixelRGB rgb) weights)
  where
    !weights = rgbLumaWeights rgb
{-# INLINE toPixelYCbCr #-}

fromPixelYCbCr ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Pixel (YCbCr cs) e'
  -> Pixel cs e
fromPixelYCbCr ycbcr = rgb
  where
    !rgb = mkPixelRGB (CM.ycbcr2rgb (coerce ycbcr :: Pixel CM.YCbCr e') weights)
    !weights = rgbLumaWeights rgb
{-# INLINE fromPixelYCbCr #-}
