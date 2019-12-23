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
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Alternative.HSV
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.HSV
  ( pattern ColorHSV
  , pattern ColorHSVA
  , pattern ColorH360SV
  , HSV
  , Color(HSV)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.HSV as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
data HSV cs

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Color (HSV cs) e = HSV (Color CM.HSV e)

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Color (HSV cs) e)
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Color (HSV cs) e)
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Color (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Color (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Color (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Color (HSV cs))
-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Color (HSV cs) e)

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Color (HSV cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSV color model
pattern ColorHSV :: e -> e -> e -> Color (HSV cs) e
pattern ColorHSV h s i = HSV (CM.ColorHSV h s i)
{-# COMPLETE ColorHSV #-}

-- | Constructor for @HSV@ with alpha channel.
pattern ColorHSVA :: e -> e -> e -> e -> Color (Alpha (HSV cs)) e
pattern ColorHSVA h s i a = Alpha (HSV (CM.ColorHSV h s i)) a
{-# COMPLETE ColorHSVA #-}


-- | Constructor for an RGB color space in an alternative HSV color model. Difference from
-- `ColorHSV` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern ColorH360SV :: RealFloat e => e -> e -> e -> Color (HSV cs) e
pattern ColorH360SV h s i <- ColorHSV ((* 360) -> h) s i where
        ColorH360SV h s i = ColorHSV (h / 360) s i
{-# COMPLETE ColorH360SV #-}

-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (HSV cs) e where
  type Components (HSV cs) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSV-" ++) . showsColorModelName (Proxy :: Proxy (Color cs e))


-- | `HSV` representation for some (@`RedGreenBlue` cs i@) color space
instance (ColorSpace cs i e, RedGreenBlue cs i) => ColorSpace (HSV cs) i e where
  type BaseColorSpace (HSV cs) = cs
  toBaseColorSpace = mkColorRGB . fmap fromDouble . CM.hsv2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsv . fmap toDouble . unColorRGB
  {-# INLINE fromBaseColorSpace #-}
