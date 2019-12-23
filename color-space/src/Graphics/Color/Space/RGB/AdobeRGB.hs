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
  ( AdobeRGB
  , D65
  , primaries
  , npmStandard
  , inpmStandard
  , transfer
  , itransfer
  , module Graphics.Color.Space
  ) where

import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.ITU.Rec601 (D65)


-- | A very common @AdobeRGB@ color space with the default `D65` illuminant
data AdobeRGB

newtype instance Color AdobeRGB e = AdobeRGB (Color CM.RGB e)

-- | Adobe`RGB` color space
deriving instance Eq e => Eq (Color AdobeRGB e)
-- | Adobe`RGB` color space
deriving instance Ord e => Ord (Color AdobeRGB e)
-- | Adobe`RGB` color space
deriving instance Functor (Color AdobeRGB)
-- | Adobe`RGB` color space
deriving instance Applicative (Color AdobeRGB)
-- | Adobe`RGB` color space
deriving instance Foldable (Color AdobeRGB)
-- | Adobe`RGB` color space
deriving instance Traversable (Color AdobeRGB)
-- | Adobe`RGB` color space
deriving instance Storable e => Storable (Color AdobeRGB e)

-- | Adobe`RGB` color space
instance Elevator e => Show (Color AdobeRGB e) where
  showsPrec _ = showsColorModel

-- | Adobe`RGB` color space
instance Elevator e => ColorModel AdobeRGB e where
  type Components AdobeRGB e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | Adobe`RGB` color space
instance Elevator e => ColorSpace AdobeRGB D65 e where
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toColorY = rgbLuminocity . fmap toRealFloat
  {-# INLINE toColorY #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}


-- | Adobe`RGB` color space
instance RedGreenBlue AdobeRGB D65 where
  gamut = primaries
  npm = npmStandard
  inpm = inpmStandard
  ecctf = fmap transfer
  {-# INLINE ecctf #-}
  dcctf = fmap itransfer
  {-# INLINE dcctf #-}

-- | sRGB normalized primary matrix. This is a helper definition, use `npm` instead.
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


-- | sRGB inverse normalized primary matrix. This is a helper definition, use `inpm` instead.
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

primaries :: RealFloat e => Gamut rgb i e
primaries = Gamut (Primary 0.64 0.33)
                  (Primary 0.21 0.71)
                  (Primary 0.15 0.06)
