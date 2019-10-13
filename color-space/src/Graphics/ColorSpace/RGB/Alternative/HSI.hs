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
-- Module      : Graphics.ColorSpace.RGB.Alternative.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.Alternative.HSI
  ( pattern PixelHSI
  , pattern PixelHSIA
  , pattern PixelH360SI
  , HSI
  , Pixel(HSI)
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.ColorModel.Alpha
import qualified Graphics.ColorModel.HSI as CM
import Graphics.ColorModel.Internal
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RGB.Internal

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
data HSI (cs :: k -> *) (i :: k)

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Pixel (HSI cs i) e = HSI (Pixel CM.HSI e)

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Pixel (HSI cs i) e)
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Pixel (HSI cs i) e)
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Pixel (HSI cs i))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Pixel (HSI cs i))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Pixel (HSI cs i))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Pixel (HSI cs i))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Pixel (HSI cs i) e)

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel (cs i) e => Show (Pixel (HSI cs i) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSI color model
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs i) e
pattern PixelHSI h s i = HSI (CM.PixelHSI h s i)
{-# COMPLETE PixelHSI #-}

-- | Constructor for @HSI@ with alpha channel.
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha (HSI cs i)) e
pattern PixelHSIA h s i a = Alpha (HSI (CM.PixelHSI h s i)) a
{-# COMPLETE PixelHSIA #-}


-- | Constructor for an RGB color space in an alternative HSI color model. Difference from
-- `PixelHSI` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern PixelH360SI :: Double -> Double -> Double -> Pixel (HSI cs i) Double
pattern PixelH360SI h s i <- PixelHSI ((* 360) -> h) s i where
        PixelH360SI h s i = PixelHSI (h / 360) s i
{-# COMPLETE PixelH360SI #-}

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel (cs i) e => ColorModel (HSI cs (i :: k)) e where
  type Components (HSI cs i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSI" ++)


-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace (cs i) e, RedGreenBlue cs i) =>
         ColorSpace (HSI cs (i :: k)) e where
  type BaseColorSpace (HSI cs i) = cs i
  toBaseColorSpace = mkPixelRGB . fmap fromDouble . CM.hsi2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsi . fmap toDouble . unPixelRGB
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = showsColorSpaceName (pure 0 :: Pixel (cs i) e)
