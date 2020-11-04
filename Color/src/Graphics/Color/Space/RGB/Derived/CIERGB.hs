{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Derived.CIERGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Derived.CIERGB
  ( CIERGB
  , castLinearity
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import qualified Graphics.Color.Space.CIE1931.RGB as CIERGB


-- | The original @CIE1931 RGB@ color space with an arbitrary illuminant
data CIERGB (i :: k) (l :: Linearity)

-- | `CIERGB` color space (derived)
newtype instance Color (CIERGB i l) e = CIERGB (Color CM.RGB e)

-- | `CIERGB` color space (derived)
deriving instance Eq e => Eq (Color (CIERGB i l) e)
-- | `CIERGB` color space (derived)
deriving instance Ord e => Ord (Color (CIERGB i l) e)
-- | `CIERGB` color space (derived)
deriving instance Functor (Color (CIERGB i l))
-- | `CIERGB` color space (derived)
deriving instance Applicative (Color (CIERGB i l))
-- | `CIERGB` color space (derived)
deriving instance Foldable (Color (CIERGB i l))
-- | `CIERGB` color space (derived)
deriving instance Traversable (Color (CIERGB i l))
-- | `CIERGB` color space (derived)
deriving instance Storable e => Storable (Color (CIERGB i l) e)

-- | `CIERGB` color space (derived)
instance (Typeable l, Illuminant i, Elevator e) => Show (Color (CIERGB (i :: k) l) e) where
  showsPrec _ = showsColorModel

-- | `CIERGB` color space (derived)
instance (Typeable l, Illuminant i, Elevator e) => ColorModel (CIERGB (i :: k) l) e where
  type Components (CIERGB i l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `CIERGB` color space (derived)
instance (Illuminant i, Typeable l, Elevator e) => ColorSpace (CIERGB i l) i e where
  type BaseModel (CIERGB i l) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . castLinearity . fmap toRealFloat
  {-# INLINE luminance #-}
  grayscale = toBaseModel . fmap fromDouble . luminance
  {-# INLINE grayscale #-}
  applyGrayscale c f = castLinearity (rgbLinearApplyGrayscale (castLinearity c) f)
  {-# INLINE applyGrayscale #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat . castLinearity
  {-# INLINE toColorXYZ #-}
  fromColorXYZ xyz = castLinearity (fromRealFloat <$> xyz2rgbLinear @(CIERGB i) xyz)
  {-# INLINE fromColorXYZ #-}

-- | `CIERGB` color space (derived)
instance Illuminant i => RedGreenBlue (CIERGB i) i where
  gamut = coerceGamut (gamut @_ @CIERGB.CIERGB)
  transfer = id
  {-# INLINE transfer #-}
  itransfer = id
  {-# INLINE itransfer #-}

-- | CIE RGB does not utilize any gamma function, therefore it is safe to cast the
-- `Linearity` kind.
--
-- @since 0.2.0
castLinearity :: Color (CIERGB i l') e -> Color (CIERGB i l) e
castLinearity = coerce
