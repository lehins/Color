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
-- Module      : Graphics.Color.Space.RGB.Alternative.HSL
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.HSL
  ( pattern ColorHSL
  , pattern ColorHSLA
  , pattern ColorH360SL
  , HSL
  , Color(HSL)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Proxy
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.HSL as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
data HSL cs

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Color (HSL cs) e = HSL (Color CM.HSL e)

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Color (HSL cs) e)
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Color (HSL cs) e)
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Color (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Color (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Color (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Color (HSL cs))
-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Color (HSL cs) e)

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Color (HSL cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative HSL color model
pattern ColorHSL :: e -> e -> e -> Color (HSL cs) e
pattern ColorHSL h s i = HSL (CM.ColorHSL h s i)
{-# COMPLETE ColorHSL #-}

-- | Constructor for @HSL@ with alpha channel.
pattern ColorHSLA :: e -> e -> e -> e -> Color (Alpha (HSL cs)) e
pattern ColorHSLA h s i a = Alpha (HSL (CM.ColorHSL h s i)) a
{-# COMPLETE ColorHSLA #-}


-- | Constructor for an RGB color space in an alternative HSL color model. Difference from
-- `ColorHSL` is that the hue is specified in 0 to 360 degree range, rather than 0 to
-- 1. Note, that this is not checked.
pattern ColorH360SL :: RealFloat e => e -> e -> e -> Color (HSL cs) e
pattern ColorH360SL h s i <- ColorHSL ((* 360) -> h) s i where
        ColorH360SL h s i = ColorHSL (h / 360) s i
{-# COMPLETE ColorH360SL #-}

-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (HSL cs) e where
  type Components (HSL cs) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("HSL-" ++) . showsColorModelName (Proxy :: Proxy (Color cs e))


-- | `HSL` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace cs i e, RedGreenBlue cs i) => ColorSpace (HSL cs) i e where
  type BaseModel (HSL cs) = CM.HSL
  type BaseSpace (HSL cs) = cs
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.hsl2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2hsl . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
