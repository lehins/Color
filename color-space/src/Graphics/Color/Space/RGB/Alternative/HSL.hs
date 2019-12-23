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
-- Module      : Graphics.Color.Space.RGB.Alternative.HSL
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.HSL
  ( pattern PixelHSL
  , pattern PixelHSLA
  , pattern PixelH360SL
  , HSL
  , Pixel(HSL)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.HSL as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
data HSL cs

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Pixel (HSL cs) e = HSL (Pixel CM.HSL e)

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Pixel (HSL cs) e)
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Pixel (HSL cs) e)
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Pixel (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Pixel (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Pixel (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Pixel (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Pixel (HSL cs) e)

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Pixel (HSL cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSL color model
pattern PixelHSL :: e -> e -> e -> Pixel (HSL cs) e
pattern PixelHSL h s i = HSL (CM.PixelHSL h s i)
{-# COMPLETE PixelHSL #-}

-- | Constructor for @HSL@ with alpha channel.
pattern PixelHSLA :: e -> e -> e -> e -> Pixel (Alpha (HSL cs)) e
pattern PixelHSLA h s i a = Alpha (HSL (CM.PixelHSL h s i)) a
{-# COMPLETE PixelHSLA #-}


-- | Constructor for an RGB color space in an alternative HSL color model. Difference from
-- `PixelHSL` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern PixelH360SL :: RealFloat e => e -> e -> e -> Pixel (HSL cs) e
pattern PixelH360SL h s i <- PixelHSL ((* 360) -> h) s i where
        PixelH360SL h s i = PixelHSL (h / 360) s i
{-# COMPLETE PixelH360SL #-}

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (HSL cs) e where
  type Components (HSL cs) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSL-" ++) . showsColorModelName (pure 0 :: Pixel cs e)


-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace cs i e, RedGreenBlue cs i) => ColorSpace (HSL cs) i e where
  type BaseColorSpace (HSL cs) = cs
  toBaseColorSpace = mkPixelRGB . fmap fromDouble . CM.hsl2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsl . fmap toDouble . unPixelRGB
  {-# INLINE fromBaseColorSpace #-}
