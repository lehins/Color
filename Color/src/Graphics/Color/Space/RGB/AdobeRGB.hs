{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.AdobeRGB
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.AdobeRGB
  ( -- * Constructors for a AdobeRGB color space.
    pattern AdobeRGB
  , pattern ColorAdobeRGB
  , pattern ColorAdobeRGBA
  , AdobeRGB
  , D65
  ) where

import Data.Typeable
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import Graphics.Color.Space.RGB.ITU.Rec601 (D65)


-- | A very common [AdobeRGB (1998)](https://en.wikipedia.org/wiki/Adobe_RGB_color_space)
-- color space with the default `D65` illuminant
--
-- \[
-- \gamma(u) = u^{2.19921875} = u^\frac{563}{256}
-- \]
--
-- \[
-- \gamma^{-1}(u) = u^\frac{1}{2.19921875} = u^\frac{256}{563}
-- \]
--
-- AdobeRGB normalized primary matrix. This is a helper definition, use `npm` instead.
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB.AdobeRGB
-- >>> npm :: NPM AdobeRGB Float
-- [ [ 0.57667000, 0.18556000, 0.18823000 ]
-- , [ 0.29734000, 0.62736000, 0.07529000 ]
-- , [ 0.02703000, 0.07069000, 0.99134000 ] ]
--
-- AdobeRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB.AdobeRGB
-- >>> inpm :: INPM AdobeRGB Float
-- [ [ 2.04159000,-0.56501000,-0.34473000 ]
-- , [-0.96924000, 1.87597000, 0.04156000 ]
-- , [ 0.01344000,-0.11836000, 1.01517000 ] ]
--
-- @since 0.1.0
data AdobeRGB (l :: Linearity)

newtype instance Color (AdobeRGB l) e = AdobeRGB (Color CM.RGB e)

-- | Constructor for a color in @AdobeRGB@ color space
--
-- @since 0.1.0
pattern ColorAdobeRGB :: e -> e -> e -> Color (AdobeRGB l) e
pattern ColorAdobeRGB r g b = AdobeRGB (CM.ColorRGB r g b)
{-# COMPLETE ColorAdobeRGB #-}

-- | Constructor for a color in @AdobeRGB@ color space with alpha channel
--
-- @since 0.1.0
pattern ColorAdobeRGBA :: e -> e -> e -> e -> Color (Alpha (AdobeRGB l)) e
pattern ColorAdobeRGBA r g b a = Alpha (AdobeRGB (CM.ColorRGB r g b)) a
{-# COMPLETE ColorAdobeRGBA #-}


-- | `AdobeRGB` color space
deriving instance Eq e => Eq (Color (AdobeRGB l) e)
-- | `AdobeRGB` color space
deriving instance Ord e => Ord (Color (AdobeRGB l) e)
-- | `AdobeRGB` color space
deriving instance Functor (Color (AdobeRGB l))
-- | `AdobeRGB` color space
deriving instance Applicative (Color (AdobeRGB l))
-- | `AdobeRGB` color space
deriving instance Foldable (Color (AdobeRGB l))
-- | `AdobeRGB` color space
deriving instance Traversable (Color (AdobeRGB l))
-- | `AdobeRGB` color space
deriving instance Storable e => Storable (Color (AdobeRGB l) e)

-- | `AdobeRGB` color space
instance (Typeable l, Elevator e) => Show (Color (AdobeRGB l) e) where
  showsPrec _ = showsColorModel

-- | `AdobeRGB` color space
instance (Typeable l, Elevator e) => ColorModel (AdobeRGB l) e where
  type Components (AdobeRGB l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | `AdobeRGB` linear color space
instance Elevator e => ColorSpace (AdobeRGB 'Linear) D65 e where
  type BaseModel (AdobeRGB 'Linear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgbLinear
  {-# INLINE fromColorXYZ #-}

-- | `AdobeRGB` color space
instance Elevator e => ColorSpace (AdobeRGB 'NonLinear) D65 e where
  type BaseModel (AdobeRGB 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | `AdobeRGB` color space
instance RedGreenBlue AdobeRGB D65 where
  gamut = Gamut (Primary 0.64 0.33)
                (Primary 0.21 0.71)
                (Primary 0.15 0.06)
  transfer u = u ** (256 / 563)
  {-# INLINE transfer #-}
  itransfer u = u ** 2.19921875 -- in rational form 563/256
  {-# INLINE itransfer #-}
  npm = NPM $ M3x3 (V3 0.57667 0.18556 0.18823)
                   (V3 0.29734 0.62736 0.07529)
                   (V3 0.02703 0.07069 0.99134)
  inpm = INPM $ M3x3 (V3  2.04159 -0.56501 -0.34473)
                     (V3 -0.96924  1.87597  0.04156)
                     (V3  0.01344 -0.11836  1.01517)
