{-# LANGUAGE UndecidableInstances #-}
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
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.Alternative.HSV
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Alternative.HSV
  ( pattern PixelHSV
  , pattern PixelHSVA
  , pattern PixelH360SI
  , HSV
  , Pixel(HSV)
  , module Graphics.ColorSpace
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.ColorModel.Alpha
import qualified Graphics.ColorModel.HSV as CM
import Graphics.ColorModel.Internal
import Graphics.ColorSpace

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
data HSV cs

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Pixel (HSV cs) e = HSV (Pixel CM.HSV e)

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Pixel (HSV cs) e)
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Pixel (HSV cs) e)
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Pixel (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Pixel (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Pixel (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Pixel (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Pixel (HSV cs) e)

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Pixel (HSV cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSV color model
pattern PixelHSV :: e -> e -> e -> Pixel (HSV cs) e
pattern PixelHSV h s i = HSV (CM.PixelHSV h s i)
{-# COMPLETE PixelHSV #-}

-- | Constructor for @HSV@ with alpha channel.
pattern PixelHSVA :: e -> e -> e -> e -> Pixel (Alpha (HSV cs)) e
pattern PixelHSVA h s i a = Alpha (HSV (CM.PixelHSV h s i)) a
{-# COMPLETE PixelHSVA #-}


-- | Constructor for an RGB color space in an alternative HSV color model. Difference from
-- `PixelHSV` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern PixelH360SI :: RealFloat e => e -> e -> e -> Pixel (HSV cs) e
pattern PixelH360SI h s i <- PixelHSV ((* 360) -> h) s i where
        PixelH360SI h s i = PixelHSV (h / 360) s i
{-# COMPLETE PixelH360SI #-}

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (HSV cs) e where
  type Components (HSV cs) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSV" ++)


-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace cs e, RedGreenBlue cs i) =>
         ColorSpace (HSV cs) e where
  type BaseColorSpace (HSV cs) = cs
  toBaseColorSpace = mkPixelRGB . fmap fromDouble . CM.hsv2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsv . fmap toDouble . unPixelRGB
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = showsColorSpaceName (pure 0 :: Pixel cs e)
