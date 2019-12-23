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
-- Module      : Graphics.Color.Space.RGB.Alternative.HSI
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.HSI
  ( pattern PixelHSI
  , pattern PixelHSIA
  , pattern PixelH360SI
  , HSI
  , Pixel(HSI)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.HSI as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space


-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
data HSI cs

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Pixel (HSI cs) e = HSI (Pixel CM.HSI e)

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Pixel (HSI cs) e)
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Pixel (HSI cs) e)
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Pixel (HSI cs))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Pixel (HSI cs))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Pixel (HSI cs))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Pixel (HSI cs))
-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Pixel (HSI cs) e)

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Pixel (HSI cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSI color model
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs) e
pattern PixelHSI h s i = HSI (CM.PixelHSI h s i)
{-# COMPLETE PixelHSI #-}

-- | Constructor for @HSI@ with alpha channel.
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha (HSI cs)) e
pattern PixelHSIA h s i a = Alpha (HSI (CM.PixelHSI h s i)) a
{-# COMPLETE PixelHSIA #-}


-- | Constructor for an RGB color space in an alternative HSI color model. Difference from
-- `PixelHSI` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern PixelH360SI :: RealFloat e => e -> e -> e -> Pixel (HSI cs) e
pattern PixelH360SI h s i <- PixelHSI ((* 360) -> h) s i where
        PixelH360SI h s i = PixelHSI (h / 360) s i
{-# COMPLETE PixelH360SI #-}

-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (HSI cs) e where
  type Components (HSI cs) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSI-" ++) . showsColorModelName (pure 0 :: Pixel cs e)


-- | `HSI` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace cs i e, RedGreenBlue cs i) => ColorSpace (HSI cs) i e where
  type BaseColorSpace (HSI cs) = cs
  toBaseColorSpace = mkPixelRGB . fmap fromDouble . CM.hsi2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsi . fmap toDouble . unPixelRGB
  {-# INLINE fromBaseColorSpace #-}
