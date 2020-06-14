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
-- Copyright   : (c) Alexey Kuleshevich 2019
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
  , primaries
  , npmStandard
  , inpmStandard
  , transfer
  , itransfer
  , module Graphics.Color.Space
  ) where

import Data.Typeable
import Data.Coerce
import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.ITU.Rec601 (D65)


-- | A very common [AdobeRGB (1998)](https://en.wikipedia.org/wiki/Adobe_RGB_color_space)
-- color space with the default `D65` illuminant
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
  gamut = primaries
  npm = npmStandard
  inpm = inpmStandard
  ecctf = AdobeRGB . fmap transfer . coerce
  {-# INLINE ecctf #-}
  dcctf = AdobeRGB . fmap itransfer . coerce
  {-# INLINE dcctf #-}

-- | AdobeRGB normalized primary matrix. This is a helper definition, use `npm` instead.
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB.AdobeRGB
-- >>> npmStandard :: NPM AdobeRGB Float
-- [ [ 0.576670, 0.185560, 0.188230 ]
-- , [ 0.297340, 0.627360, 0.075290 ]
-- , [ 0.027030, 0.070690, 0.991340 ] ]
--
-- @since 0.1.0
npmStandard :: RealFloat e => NPM AdobeRGB e
npmStandard = NPM $ M3x3 (V3 0.57667 0.18556 0.18823)
                         (V3 0.29734 0.62736 0.07529)
                         (V3 0.02703 0.07069 0.99134)


-- | AdobeRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
--
-- >>> :set -XDataKinds
-- >>> import Graphics.Color.Space.RGB.AdobeRGB
-- >>> inpmStandard :: INPM AdobeRGB Float
-- [ [ 2.041590,-0.565010,-0.344730 ]
-- , [-0.969240, 1.875970, 0.041560 ]
-- , [ 0.013440,-0.118360, 1.015170 ] ]
--
-- @since 0.1.0
inpmStandard :: RealFloat e => INPM AdobeRGB e
inpmStandard = INPM $ M3x3 (V3  2.04159 -0.56501 -0.34473)
                           (V3 -0.96924  1.87597  0.04156)
                           (V3  0.01344 -0.11836  1.01517)



-- | AdobeRGB transfer function "gamma":
--
-- \[
-- \gamma(u) = u^{2.19921875} = u^\frac{563}{256}
-- \]
--
-- @since 0.1.0
transfer :: Floating a => a -> a
transfer u = u ** (256 / 563)
{-# INLINE transfer #-}


-- | AdobeRGB inverse transfer function "gamma":
--
-- \[
-- \gamma^{-1}(u) = u^\frac{1}{2.19921875} = u^\frac{256}{563}
-- \]
--
-- @since 0.1.0
itransfer :: Floating a => a -> a
itransfer u = u ** 2.19921875 -- in rational form 563/256
{-# INLINE itransfer #-}

-- | @AdobeRGB@ primaries
--
-- @since 0.1.0
primaries :: RealFloat e => Gamut rgb i e
primaries = Gamut (Primary 0.64 0.33)
                  (Primary 0.21 0.71)
                  (Primary 0.15 0.06)
