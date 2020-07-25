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
-- Module      : Graphics.Color.Space.CIE1931.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.CIE1931.RGB
  ( CIERGB
  , primaries
  , castLinearity
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Illuminant.CIE1931


-- | The original @CIE1931 RGB@ color space with an arbitrary illuminant
data CIERGB (l :: Linearity)

-- | `CIERGB` color space
newtype instance Color (CIERGB l) e = CIERGB (Color CM.RGB e)

-- | `CIERGB` color space
deriving instance Eq e => Eq (Color (CIERGB l) e)
-- | `CIERGB` color space
deriving instance Ord e => Ord (Color (CIERGB l) e)
-- | `CIERGB` color space
deriving instance Functor (Color (CIERGB l))
-- | `CIERGB` color space
deriving instance Applicative (Color (CIERGB l))
-- | `CIERGB` color space
deriving instance Foldable (Color (CIERGB l))
-- | `CIERGB` color space
deriving instance Traversable (Color (CIERGB l))
-- | `CIERGB` color space
deriving instance Storable e => Storable (Color (CIERGB l) e)

-- | `CIERGB` color space
instance (Typeable l, Elevator e) => Show (Color (CIERGB l) e) where
  showsPrec _ = showsColorModel

-- | `CIERGB` color space
instance (Typeable l, Elevator e) => ColorModel (CIERGB l) e where
  type Components (CIERGB l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `CIERGB` linear color space
instance (Typeable l, Elevator e) => ColorSpace (CIERGB l) 'E e where
  type BaseModel (CIERGB l) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . castLinearity . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat . castLinearity
  {-# INLINE toColorXYZ #-}
  fromColorXYZ xyz = castLinearity (fromRealFloat <$> xyz2rgbLinear @CIERGB xyz)
  {-# INLINE fromColorXYZ #-}

-- | `CIERGB` color space
instance RedGreenBlue CIERGB 'E where
  gamut = primaries
  ecctf = coerce
  {-# INLINE ecctf #-}
  dcctf = coerce
  {-# INLINE dcctf #-}

-- | CIE RGB does not utilize any gamma function, therefore it is safe to cast the
-- `Linearity` kind.
--
-- @since 0.2.0
castLinearity :: Color (CIERGB l') e -> Color (CIERGB l) e
castLinearity = coerce


-- | Primaries for CIERGB
--
-- @since 0.2.0
primaries :: RealFloat e => Gamut rgb i e
primaries = Gamut (Primary 0.734742840005998 0.265257159994002)
                  (Primary 0.273779033824958 0.717477700256116)
                  (Primary 0.166555629580280 0.008910726182545)
-- Source ^: https://github.com/colour-science/colour/blob/60679360c3990bc549b5f947bfeb621383e18b5e/colour/models/rgb/datasets/cie_rgb.py#L44-L48
